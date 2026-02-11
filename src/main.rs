use getopts_macro::getopts_options;
use mtsx_ls::Tracer;
use std::collections::HashMap;

use anyhow::{Result, anyhow, bail};
use crossbeam_channel::{Receiver, Sender};
use lsp_server::{IoThreads, Message};
use lsp_types::{CodeActionKind, CodeActionOptions, CompletionOptions, InitializeParams, InitializeResult, MessageType, PublishDiagnosticsParams, ServerCapabilities, ShowMessageParams, TextDocumentSyncCapability, TextDocumentSyncKind, TraceValue, Uri, notification::{self, Notification}, request::{self, Request}};

fn main() {
    let options = getopts_options! {
        -h, --help          "show help message";
        -v, --version       "show version";
    };
    let matches = getopts_macro::simple_parse(
        &options,
        "MT-Manager Language Server",
        0,
        "",
    );
    if matches.opt_present("version") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return;
    }

    main_loop(&matches).unwrap();
}

fn main_loop(_matches: &getopts_macro::getopts::Matches) -> Result<()> {
    let (connect, io) = lsp_server::Connection::stdio();
    let _io = IoJoiner(Some(io));
    let server_capabilities = ServerCapabilities {
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_owned(), "-".to_owned(), "\"".to_owned()]),
            ..Default::default()
        }),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        code_action_provider: Some(lsp_types::CodeActionProviderCapability::Options(CodeActionOptions {
            code_action_kinds: Some([CodeActionKind::EMPTY].into()),
            ..Default::default()
        })),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        references_provider: Some(lsp_types::OneOf::Left(true)),
        ..Default::default()
    };
    let init_params = {
        let (id, params) = connect.initialize_start()?;

        let initialize_data = InitializeResult {
            capabilities: server_capabilities,
            server_info: Some(lsp_types::ServerInfo {
                name: env!("CARGO_PKG_NAME").into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        };

        let initialize_data = serde_json::to_value(initialize_data)?;
        connect.initialize_finish(id, initialize_data)?;

        params
    };
    let InitializeParams {
        workspace_folders,
        trace,
        ..
    } = serde_json::from_value::<InitializeParams>(init_params)?;
    let _workspace_folders = workspace_folders.ok_or(anyhow!("Cannot find workspace folder"))?;
    let mut ctx = Ctx::new(connect.sender, connect.receiver);
    ctx.tracer.trace = !matches!(trace, None | Some(TraceValue::Off));
    ctx.run().map_err(|e| { ctx.trace(&e); e })
}

struct IoJoiner(pub Option<IoThreads>);
impl std::ops::DerefMut for IoJoiner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}
impl std::ops::Deref for IoJoiner {
    type Target = IoThreads;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}
impl Drop for IoJoiner {
    fn drop(&mut self) {
        if let Some(io) = self.0.take() {
            io.join().unwrap()
        }
    }
}
impl IoJoiner {
    fn _consume(mut self) -> IoThreads {
        self.0.take().unwrap()
    }
}

struct Ctx {
    open_files: HashMap<Uri, String>,
    sender: Sender<Message>,
    recver: Receiver<Message>,
    tracer: Tracer,
}

impl Ctx {
    fn new(sender: Sender<Message>, recver: Receiver<Message>) -> Self {
        Self {
            open_files: Default::default(),
            sender,
            recver,
            tracer: Tracer::new(env!("CARGO_BIN_NAME")),
        }
    }

    fn run(&mut self) -> Result<(), anyhow::Error> {
        while let Ok(msg) = self.recver.recv() {
            match msg {
                Message::Request(request) => self.handle_requests(request)?,
                Message::Response(response) => {
                    let response = &mut Some(response);

                    if let Some(response) = response {
                        self.trace(format_args!("unknown response {response:?}"));
                    }
                },
                Message::Notification(notification) => {
                    let notification = &mut Some(notification);
                    self.try_handle_notif::<notification::DidOpenTextDocument>(notification)?;
                    self.try_handle_notif::<notification::DidChangeTextDocument>(notification)?;
                    self.try_handle_notif::<notification::DidCloseTextDocument>(notification)?;

                    if let Some(notification) = notification {
                        self.trace(format_args!("unknown notification {notification:#?}"));
                    }
                },
            }
        }
        Ok(())
    }

    fn handle_requests(&mut self, request: lsp_server::Request) -> Result<()> {
        let request = &mut Some(request);

        self.try_handle_req::<request::Completion>(request)?;
        self.try_handle_req::<request::HoverRequest>(request)?;
        self.try_handle_req::<request::CodeActionRequest>(request)?;
        self.try_handle_req::<request::GotoDefinition>(request)?;
        self.try_handle_req::<request::References>(request)?;

        if let Some(request) = request {
            bail!("unknown request {request:#?}")
        }
        Ok(())
    }

    fn try_handle_notif<T: NotificationHandler>(&mut self, notification: &mut Option<lsp_server::Notification>) -> Result<()> {
        let Some(lsp_server::Notification { params, .. })
            = notification.take_if(|it| it.method == T::METHOD) else { return Ok(()) };
        let params = match serde_json::from_value(params) {
            Ok(it) => it,
            Err(err) => bail!(err),
        };
        T::handle(self, params)
    }

    fn try_handle_req<T: RequestHandler>(&mut self, request: &mut Option<lsp_server::Request>) -> Result<()> {
        let Some(lsp_server::Request { id, params, .. })
            = request.take_if(|req| req.method == T::METHOD) else { return Ok(()) };
        self.trace(format_args!("received request {} : {params:#?}", T::METHOD));
        let params = match serde_json::from_value(params) {
            Ok(it) => it,
            Err(err) => return Err(err.into()),
        };
        let result = T::handle(self, params);
        let response = match result {
            Ok(value) => {
                match serde_json::to_value(value) {
                    Ok(to_value) => lsp_server::Response { id, result: Some(to_value), error: None },
                    Err(e) => return Err(anyhow!(e)),
                }
            },
            Err(err) => lsp_server::Response {
                id,
                result: None,
                error: Some(lsp_server::ResponseError {
                    code: 1,
                    message: err.to_string(),
                    data: None,
                }),
            },
        };
        self.trace(format_args!("send response {} : {response:#?}", T::METHOD));
        let err = response.error.clone();
        self.sender.send(Message::Response(response))?;
        if let Some(err) = err {
            self.send_window_notif(MessageType::ERROR, err.message)?;
        }
        Ok(())
    }

    fn read_file(&self, uri: &Uri) -> &str {
        self.open_files.get(uri).unwrap_or_else(|| {
            self.trace(format_args!("cannot read unknown uri: {uri:?}"));
            const { &String::new() }
        })
    }

    fn send_window_notif(&self, typ: MessageType, msg: impl std::fmt::Display) -> Result<()> {
        let params = ShowMessageParams {
            typ,
            message: msg.to_string(),
        };
        self.send_notif::<notification::ShowMessage>(params)
    }

    fn send_notif<T: Notification>(&self, params: T::Params) -> Result<()> {
        let params = serde_json::to_value(params)?;
        let msg = Message::Notification(lsp_server::Notification {
            method: T::METHOD.to_owned(),
            params,
        });
        self.sender.send(msg)?;
        Ok(())
    }

    fn trace(&self, s: impl std::fmt::Display) {
        self.tracer.trace(s);
    }
}

trait RequestHandler: Request {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<Self::Result>;
}
impl RequestHandler for request::Completion {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<Self::Result> {
        let file = ctx.read_file(&param.text_document_position.text_document.uri);
        let at = param.text_document_position.position;
        Ok(Some(lsp_types::CompletionResponse::Array(mtsx_ls::completions(file, at, &ctx.tracer))))
    }
}
impl RequestHandler for request::HoverRequest {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<Self::Result> {
        let file = ctx.read_file(&param.text_document_position_params.text_document.uri);
        let at = param.text_document_position_params.position;
        Ok(mtsx_ls::hover_doc(file, at).map(|(docs, range)| {
            lsp_types::Hover {
                contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::LanguageString(lsp_types::LanguageString {
                    language: "mtsyntax".to_owned(),
                    value: docs,
                })),
                range: Some(range),
            }
        }))
    }
}
impl RequestHandler for request::GotoDefinition {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<Self::Result> {
        let uri = param.text_document_position_params.text_document.uri;
        let at = param.text_document_position_params.position;
        let file = ctx.read_file(&uri);

        Ok(mtsx_ls::goto_define(file, at).map(|range| {
            lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location { uri, range },
            )
        }))
    }
}
impl RequestHandler for request::References {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<Self::Result> {
        let uri = param.text_document_position.text_document.uri;
        let at = param.text_document_position.position;
        let file = ctx.read_file(&uri);

        let references = mtsx_ls::references(file, at).map(|it| {
            it.into_iter().map(|range| lsp_types::Location { uri: uri.clone(), range }).collect()
        });
        Ok(references)
    }
}
impl RequestHandler for request::CodeActionRequest {
    fn handle(_ctx: &mut Ctx, _param: Self::Params) -> Result<Self::Result> {
        Ok(Some(vec![]))
    }
}

trait NotificationHandler: Notification {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<()>;
}
impl NotificationHandler for notification::DidOpenTextDocument {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<()> {
        let lsp_types::TextDocumentItem { uri, text, .. } = param.text_document;
        ctx.trace(format_args!("open file {}", uri.as_str()));

        ctx.send_notif::<notification::PublishDiagnostics>(PublishDiagnosticsParams {
            diagnostics: mtsx_ls::diagnostics(&text),
            uri: uri.clone(),
            version: Some(param.text_document.version),
        })?;

        let file = ctx.open_files.entry(uri).or_default();
        *file = text;

        Ok(())
    }
}
impl NotificationHandler for notification::DidChangeTextDocument {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<()> {
        let uri = param.text_document.uri;
        if !ctx.open_files.contains_key(&uri) {
            ctx.open_files.insert(uri.clone(), String::new());
        }
        ctx.trace(format_args!("change file {}", uri.as_str()));
        let file = ctx.open_files.get_mut(&uri).unwrap();

        for change in param.content_changes {
            if change.range.is_some() {
                bail!("unsupported range change sync: {change:#?}")
            }
            *file = change.text;
        }

        // vscode unsupported pull diagnostics model
        let diagnostics = mtsx_ls::diagnostics(file);
        ctx.send_notif::<notification::PublishDiagnostics>(PublishDiagnosticsParams {
            diagnostics,
            uri,
            version: Some(param.text_document.version),
        })?;
        Ok(())
    }
}
impl NotificationHandler for notification::DidCloseTextDocument {
    fn handle(ctx: &mut Ctx, param: Self::Params) -> Result<()> {
        let uri = param.text_document.uri;
        ctx.trace(format_args!("close file {}", uri.as_str()));
        if ctx.open_files.remove(&uri).is_none() {
            ctx.send_window_notif(MessageType::WARNING, format_args!("Cannot close unknown file: {uri:?}"))?;
        }
        Ok(())
    }
}
