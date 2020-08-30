///

enum StatementType {
    Noop,
    Print,
    Assign,
    If,
    Goto,
}

struct StatementNode {
    type_: StatementType,
}

struct ValueNode {
    name: String,
    value: i64,
}

enum ArithmeticOperator {
    Plus,
    Minus,
    Mult,
    Div,
}

struct PrintStatement {
    id: ValueNode,
}

// struct GotoStatement {
//   target:
// }
//
