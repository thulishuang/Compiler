完成：
	独立主程序
	解释器
	文法解析

	REPL
	PrettyPrint

	逻辑表达式
	浮点算数表达式
	字符串与列表
	WHILE语言

独立主程序中的解释执行与REPL操作方法

1.在compiler目录下运行stack build命令得到主程序

2.运行stack exec compiler -- --in input.txt --out output.txt 命令：
	--in指定输入文件的路径。--out 指定将解释的结果输出到哪个文件。 如果不指定--out选项，那么结果应输出到stdout 

3.运行stack exec compiler -- --tree input.txt --out output.txt 命令：
	--tree后跟程序文件的路径。该命令输出指定程序的抽象语法树到指定文件或stdout。 

4.运行stack exec compiler -- --repl 命令 进入repl 模式：
	:i <program> 将程序执行结果输出到stdout
	:t  输出上一段程序的语法抽象树
	:q  退出解释器程序