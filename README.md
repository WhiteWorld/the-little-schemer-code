the-little-schemer-code
=======================

Code in The Little Schemer ,using racket(DrRacket)


## 设置环境

1. DrRacket 选择 "由源代码来确定语言"
2. 文件首添加 `#lang racket`
3. 文件首添加atom的定义
    
    ```Racket
    (define atom?
        (lambda (a)
            (not (list? a))))
                (quote a)
    ```
4. 



