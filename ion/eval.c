int64_t eval_unary_int64(TokenKind op, int64_t operand) {
    int64_t result;
    switch (op) {
    case TOKEN_ADD:
        result = +operand;
        break;
    case TOKEN_SUB:
        result = -operand;
        break;
    case TOKEN_MUL:
    case TOKEN_AND:
        result = 0;
        break;
    default:
        result = 0;
        assert(0);
        break;
    }
    return result;
}

int64_t eval_binary_int64(TokenKind op, int64_t left, int64_t right) {
    int64_t result;
    switch (op) {
    case TOKEN_MUL:
        result = left * right;
        break;
    case TOKEN_DIV:
        result = right != 0 ? left / right : 0;
        break;
    case TOKEN_MOD:
        result = right != 0 ? left % right : 0;
        break;
    case TOKEN_AND:
        result = left & right;
        break;
    case TOKEN_LSHIFT:
        result = left << right;
        break;
    case TOKEN_RSHIFT:
        result = left >> right;
        break;
    case TOKEN_ADD:
        result = left + right;
        break;
    case TOKEN_SUB:
        result = left - right;
        break;
    case TOKEN_XOR:
        result = left ^ right;
        break;
    case TOKEN_OR:
        result = left | right;
        break;
    case TOKEN_EQ:
        result = left == right;
        break;
    case TOKEN_NOTEQ:
        result = left != right;
        break;
    case TOKEN_LT:
        result = left < right;
        break;
    case TOKEN_LTEQ:
        result = left <= right;
        break;
    case TOKEN_GT:
        result = left > right;
        break;
    case TOKEN_GTEQ:
        result = left >= right;
        break;
    default:
        result = 0;
        assert(0);
        break;
    }
    return result;
}

