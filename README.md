用于 MT 管理器语法文件 (.mtsx) 的语言服务器(LS), 使用语言服务器协议 (LSP)

# 支持特性
- [x] 补全字段
- [x] 补全内建匹配器、格式化器、收缩器
- [x] 补全内建模式助手函数
- [x] 补全内建颜色
- [x] 补全用户颜色
- [x] 补全引入匹配器
- [x] 补全引入正则
- [x] 检查重复字段
- [ ] 检查未知字段
- [x] 查看字段文档
- [x] 查看用户定义的匹配器、正则、颜色与文档
- [x] 跳转用户定义的匹配器、正则、颜色的定义
- [x] 跳转用户定义的匹配器、正则、颜色的引用
- [x] 重命名用户定义的匹配器、正则、颜色
- [x] 提取匹配器与正则至定义
- [x] 从引用内联用户定义的匹配器与正则

# 编辑器
可执行文件与扩展可以在 Releases 中找到

- VIM : 使用 [mtsyntax.vim](https://github.com/A4-Tacks/mtsyntax.vim) 获取语法高亮,
        配置将 mtsx-ls 的可执行文件和文件类型关联即可
- VSCode : 使用提供的扩展 (.vsix), 并将 mtsx-ls 放入 PATH 环境变量所包含的文件夹中即可

# Code Action
以下使用 `$0` 表示光标

- `$0{match: /x/}` -> `[{match: /x/}]`
- `{match: include($0"x")}` -> `{match: /x/}` (内联 defines 中定义的模式)
- `{include: $0"x"}` -> `{match: /x/}` (内联 defines 中定义的匹配器)
- `{$0match: /x/}` -> `{include: "new_name"}` (提取匹配器至 defines 中, 需要存在非空 defines)
- `{match: $0/x/}` -> `{match: include("new_name")}` (提取模式至 defines 中)
