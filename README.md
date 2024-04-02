# Xenon ATC-X

Xenon 是某大学编译原理课程的作业，其目标是把 SysY 编译为 RISC-V 汇编. SysY 是 C 语言的一个子集.

Xenon 已被废弃，Xenon ATC-X 是其后继项目. ATC-X = Advanced Technology Complier-X（先进技术编译器-X）

此外，Xenon 包含一些语言扩展：

1. 自定义中缀运算符. 例如：
    ```c
    int f(int x, int y) { return (x - 1) * (y - 1); }
    int main()
    {
        int x = 2, y = 2;
        return x ~f~ y; // 等价于 return f(x, y);
    }
    ```
2. 各类复合赋值运算符，如 `+=`，`*=`.
3. 复合运算符返回左值，这意味着可以写 `x = y = z;` 这种代码.
4. 自增自减运算符，其中前缀自增自减返回左值. 例如 `++x = 1;`
5. 数组初始化允许使用空初始化列表.
4. 以常量下标取得的常量数组元素可以参与编译期常量表达式求值.

Xenon ATC-X 的开发仍处于早期阶段，它目前包含：

- 使用有限状态机的预处理器，可以去除源代码中的注释，以及把多个 `\` 结尾物理行拼接为一个逻辑行.
- 使用 [pest](https://pest.rs) 为解析器生成器，[Koopa IR](https://github.com/pku-minic/koopa) 为中间表示的前端. 这里，[pest](https://pest.rs) 是一种基于解析表达式文法的解析器生成器，[Koopa IR](https://github.com/pku-minic/koopa) 是专为某大学编译器课程开发的中间表示.

目前，Xenon ATC-X 的工作步骤为：

1. 读入文件，将所有的 `\r\n` 转为 `\n`.
2. 1 中的字符串预处理器，得到不包含注释的源代码.
3. 2 中的字符串送入 [`SysYParser`](src/frontend/parser.rs)，得到 [pest](https://pest.rs) 中以 `Pairs` 为基础的“动态类型”语法树.
4. [`parser`](src/frontend/parser.rs) 深度优先遍历 3 中的语法树，得到在 [`ast`](src/frontend/ast.rs) 中定义的翻译单元类型. 同时，这一步还会进行编译期常量表达式的求值、表达式类型检查、符号重定义检查、数组初始化列表检查、简易的控制流化简和表达式化简.
5. 检查后的翻译单元送入 [`ir_generator`](src/frontend/ir_generator.rs)，直接生成文本格式的 [Koopa IR](https://github.com/pku-minic/koopa).
