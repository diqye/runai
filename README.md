# runai

## build

`cd` current project directory and run as bellow
```
stack install
```
## run
```
runai your/path.yaml
```

**yaml config example**
`code.yaml`
```yaml
odel: "gpt-4o"
# 微软GPT配置
azure:
  # KEY 从环境变量中获取 , 例： 9c0cxxxxx033
  key: $AZURE_KEY
  # ENDPOINT 从环境变量中获取 , 例： https://xxxx.azure.com
  endpoint: $AZURE_ENDPOINT
  temperature: 0
messages:
- system: 你是英语老师
- user: 在一个句子中，我划出不懂的单词，讲解在句子中的含义给出音标
- assistant: 好的
- user: so you might be wondering what the purpose of constexpr is ? | wondering purpose
- assistant: |

    so you might be wondering what the purpose of constexpr is
    所以，你可能会好奇`constexpr` 的用途时什么?

    - wondering /ˈwʌndərɪŋ/ 动词（动名词形式) 对某事感到疑惑或好奇
    - purpose /ˈpɜːrpəs/ 名词 用途
```

## Example

![png](./asserts/example.png)


## help
```
~/p/runai (master|✔) $ runai
使用: runai your/path.yaml
在会话中(Tab补全)
> :export         将会话导出文件为export.yaml
> :start          直接调用LLM
> :clear          清空会话
> :               进入/退出多行模式
> :reload         从新加载当前配置文件
> :change file.yaml 加载新的配置文件
```