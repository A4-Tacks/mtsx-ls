import {
  LanguageClient,
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(_ctx: any) {
  client = new LanguageClient(
    'bangls',
    'Bang Language Server',
    { command: 'mtsx-ls', args: [] },
    { documentSelector: [{scheme: 'file', language: 'mtsyntax'}] }
  );
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
