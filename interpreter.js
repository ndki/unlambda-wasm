// here's an interpreter for Unlambda!
// call Unlambda.parse(<string>) to make a state machine,
// and then call .run() on that to execute it!
export const Unlambda = class {
    parse (source) {
        let states = new StateData ();
        let last_fn_ptr;
        let lefts = [];
        let left_completed = [];
        let char_fn;
        let char_result = void 8;
        let print_cache = new Map ();
        let compare_cache = new Map ();
        const READ = 0;
        const CHAR = 1;
        const CMMT = 2;
        let state = READ;
        for (let c of source) {
            if (state == READ && c == '#') {
                state = CMMT;
            } else {
                switch (state) {
                    case CMMT:
                    if (c == "\n") state = READ;
                    break;
                    case CHAR:
                    let cache = char_fn == FunctionId.Print ?
                        print_cache : compare_cache;
                    char_result = cache.get(c);
                    if (!char_result) {
                        char_result = new CharFnNode(char_fn, c);
                        cache.set(c, char_result);
                    }
                    state = READ;
                    // continue
                    case READ:
                    if (char_result || c.match(/\S/)) {
                        if (left_completed.length == 0 && lefts.length) {
                            throw 'Parse error: Unexpected character "'+c+'" after end of program.'+"\n"
                                + 'Are you missing a function application at the beginning?';
                        }
                        if (c == '`') {
                            // defer until we know what we're doing
                            left_completed.push(false);
                        } else if (c == '.') {
                            char_fn = FunctionId.Print;
                            state = CHAR;
                        } else if (c == '?') {
                            char_fn = FunctionId.Compare;
                            state = CHAR;
                        } else {
                            let fn;
                            if (char_result !== undefined) {
                                fn = char_result;
                                char_result = void 8;
                            } else if ((fn = char2fn[c]) === undefined) {
                                throw 'Parse error: Unrecognized character '+c+'.'
                            }
                            if (left_completed.length == 0) {
                                // program is just a single function, apparently
                                let last_rhs = states.add_resolved(SymbolFn.Identity, fn);
                                lefts.push(last_rhs);
                                last_fn_ptr = last_rhs;
                            } else if (left_completed[left_completed.length-1]) {
                                // complete function application using left and right! :)
                                // then we also need to clean up those functions that were
                                // waiting on a right-hand side.
                                let last_rhs = fn;
                                while (left_completed[left_completed.length-1]) {
                                    left_completed.pop();
                                    last_rhs = states.add_application(lefts.pop(), last_rhs)
                                }
                                lefts.push(last_rhs);
                                left_completed[left_completed.length-1] = true;
                                last_fn_ptr = last_rhs;
                            } else {
                                // defer until we get rhs
                                lefts.push(fn);
                                left_completed[left_completed.length-1] = true;
                            }
                        }
                    }
                }
            }
        }
        console.log(''+states);
        let state_machine = new StateMachine (states, last_fn_ptr);
        return state_machine;
    }
}

// we compile to a state machine...  we very frequently add states.
// each state represents some function application,
// although a Chain state implicitly represents the application of
// <`NQ>, where Q is a normal continuation, and N is function that
// calls that continuation, but with a given chain of continuations
// (in other words, it represents a node of the callstack).
// there are only a few types of function:
// one of the symbol-functions, one of the character-functions,
// the special Variable function, and the special Pointer function.
// the Variable special function resolves to the value of 
// the last function application. it is the access point for
// our single internal variable other than "current character".
// the Pointer special function is typed. so a Pointer to a Constant
// state is a Constant Pointer. more importantly, a Pointer to
// Exit or Chain is a continuation, a Pointer to Call_ is an unresolved
// function application and thus has no known value, and a Pointer to
// anything else has the normal value of whatever it refers to as a function.
// as far as the states go,
// ............................... hm, i might explain that later.
// Note: it seems really weird to me that "current character" is
// a "global" variable. i feel like reseting the callstack should
// also reset the "current character" value. but we follow the
// other implementations.
const StateType = {
    Exit: 'E',
    CallLeft: 'U',
    CallRight: 'V',
    CallBoth: 'W',
    Resolved: 'R',
    Chain: '&',
    Identity: 'I',
    Constant: 'K',
    Substitute: 'S',
    Substitute2: 'Z',
    Promise: 'D',
}
const FunctionId = {
    Unknown: '!',
    Variable: 'X',
    Identity: 'i',
    Void: 'v',
    Delay: 'd',
    Constant: 'k',
    Substitute: 's',
    CallCC: 'c',
    Read: '@',
    Reprint: '|',
    Pointer: 'P',
    Print: '.',
    Compare: '?',
}

const FnNode = class {
    constructor (id = FunctionId.Unknown) { this.id = id }
    toString () { return this.id }
}
const SymbolFn = {
    Variable: new FnNode(FunctionId.Variable),
    Identity: new FnNode(FunctionId.Identity),
    Void: new FnNode(FunctionId.Void),
    Delay: new FnNode(FunctionId.Delay),
    Constant: new FnNode(FunctionId.Constant),
    Substitute: new FnNode(FunctionId.Substitute),
    CallCC: new FnNode(FunctionId.CallCC),
    Read: new FnNode(FunctionId.Read),
    Reprint: new FnNode(FunctionId.Reprint),
}
const CharFnNode = class extends FnNode {
    constructor (id, value) { super(id); this.value = value }
    toString () { return this.id+this.value }
}
const char2fn = {
    i: SymbolFn.Identity,
    v: SymbolFn.Void,
    d: SymbolFn.Delay,
    k: SymbolFn.Constant,
    s: SymbolFn.Substitute,
    c: SymbolFn.CallCC,
    r: new CharFnNode(FunctionId.Print, "\n"),
    '@': SymbolFn.Read,
    '|': SymbolFn.Reprint,
}
const PtrFn = class extends FnNode {
    type; addr;
    constructor (type, addr) {
        super(FunctionId.Pointer);
        this.type = type;
        this.addr = addr;
    }
    toString () {
        return 'P['+this.type+this.addr+']'
    }
}
const EXIT = new PtrFn(StateType.Exit,0);

const State = class {
    left; right;
    toString () {
        return this.right ? this.left ? '<'+this.left+' '+this.right+'>'
            : '<'+this.right+'>' : '<>'
    }
}

const StateData = class {
    chain_states = [];
    call_states = [];
    resolved_states = [];
    identity_states = [];
    simple_states = [];
    substitute_states = [];
    application_cache = new Map();
    size () {
        return this.call_states.length/2 + this.resolved_states.length/2
            + this.chain_states.length/2 + this.identity_states.length
            + this.simple_states.length + this.substitute_states.length/2
    }
    clone () {
        let sd = new StateData();
        sd.call_states = this.call_states;
        sd.resolved_states = this.resolved_states;
        sd.chain_states = this.chain_states;
        sd.identity_states = this.identity_states;
        sd.simple_states = this.simple_states;
        sd.substitute_states = this.substitute_states;
        return sd;
    }
    add_application (x, y) {
        let cache = this.application_cache.get(x);
        if (!cache) {
            cache = new Map ();
            this.application_cache.set(x, cache);
        }
        let ptr = cache.get(y);
        if (ptr) {
        } else if (x.id == FunctionId.Pointer) {
            if (y.id == FunctionId.Pointer) {
                ptr = this.add_call_both(x, y);
            } else {
                ptr = this.add_call_left(x, y);
            }
        } else if (y.id == FunctionId.Pointer) {
            ptr = this.add_call_right(x, y);
        } else {
            ptr = this.add_resolved(x, y);
        }
        cache.set(y, ptr);
        return ptr;
    }
    add_call_left (x, y) {
        let i = this.call_states.push(x, y)-2;
        return new PtrFn(StateType.CallLeft, i);
    }
    add_call_right (x, y) {
        let i = this.call_states.push(x, y)-2;
        return new PtrFn(StateType.CallRight, i);
    }
    add_call_both (x, y) {
        let i = this.call_states.push(x, y)-2;
        return new PtrFn(StateType.CallBoth, i);
    }
    get_call ({addr}) {
        return {left: this.call_states[addr], right: this.call_states[addr+1]}
    }
    add_chain (x, y) {
        let i = this.chain_states.push(x, y)-2;
        return new PtrFn(StateType.Chain, i);
    }
    get_chain ({addr}) {
        return {left: this.chain_states[addr], right: this.chain_states[addr+1]}
    }
    add_resolved (x, y) {
        let i = this.resolved_states.push(x, y)-2;
        return new PtrFn(StateType.Resolved, i);
    }
    get_resolved ({addr}) {
        return {left: this.resolved_states[addr], right: this.resolved_states[addr+1]}
    }
    add_simple (t, x) {
        let i = this.simple_states.push(x)-1;
        return new PtrFn(t, i)
    }
    add_constant (x) {
        return this.add_simple(StateType.Constant, x);
    }
    get_constant ({addr}) {
        return this.simple_states[addr]
    }
    add_identity (x) {
        return this.add_simple(StateType.Identity, x);
    }
    get_identity ({addr}) {
        return this.simple_states[addr]
    }
    add_promise (x) {
        return this.add_simple(StateType.Promise, x);
    }
    get_promise ({addr}) {
        return this.simple_states[addr]
    }
    add_substitute (x) {
        return this.add_simple(StateType.Substitute, x);
    }
    get_substitute ({addr}) {
        return this.simple_states[addr]
    }
    add_substitute2 (x, y) {
        let i = this.substitute_states.push(x, y)-2;
        return new PtrFn(StateType.Substitute2, i);
    }
    get_substitute2 ({addr}) {
        return {left: this.substitute_states[addr], right: this.substitute_states[addr+1]}
    }
    toString () {
        return '&:' + this.chain_states + "\n"+
            'C:' + this.call_states + "\n"+
            'R:' + this.resolved_states + "\n"+
            'I:' + this.identity_states + "\n"+
            'S:' + this.simple_states + "\n"+
            'Z:' + this.substitute_states + "\n";
    }
}

const StateMachine = class {
    variable = EXIT;
    current_char = SymbolFn.Void;
    trail = EXIT;
    ptr; states;
    exit = false;
    steps = 0;
    constructor (states, init) {
        this.states = states;
        this.ptr = init;
    }
    run (input, output) {
        this.exit = false;
        let states = this.states;
        while (this.ptr.type != StateType.Exit && !this.exit) {
            this.steps++;
            switch (this.ptr.type) {
                //console.log(''+this.ptr+' <'+call.left+' '+call.right+'> <- '+this.variable);
                case StateType.CallLeft:
                let call_left = states.get_call(this.ptr);
                this.ptr = call_left.left;
                // adding a chain ensures we come back to <replacement> after we resolve call.left
                let left_replacement= states.add_resolved(SymbolFn.Variable, call_left.right);
                this.trail = states.add_chain(left_replacement, this.trail);
                break;
                case StateType.CallRight:
                let call_right = states.get_call(this.ptr);
                let call_rl = call_right.left;
                if (call_rl.id == FunctionId.Variable) { call_rl = this.variable }
                if (call_rl.id == FunctionId.Delay) {
                    this.variable = states.add_promise(call_right.right);
                    this.ptr = this.trail;
                } else {
                    this.ptr = call_right.right;
                    let right_replacement = states.add_resolved(call_rl, SymbolFn.Variable);
                    this.trail = states.add_chain(right_replacement, this.trail);
                }
                break;
                case StateType.CallBoth:
                let call_both = states.get_call(this.ptr);
                this.ptr = call_both.left;
                let replacement = states.add_call_right(SymbolFn.Variable, call_both.right);
                this.trail = states.add_chain(replacement, this.trail);
                break;
                case StateType.Resolved:
                let rsv = states.get_resolved(this.ptr);
                //console.log(''+this.ptr+' ['+rsv.left+' '+rsv.right+'] <- '+this.variable);
                if (rsv.left.id == FunctionId.Variable) { rsv.left = this.variable }
                else if (rsv.right.id == FunctionId.Variable) { rsv.right = this.variable }
                // the function determines what we do
                switch (rsv.left.id) {
                    // tentatively putting void first since it
                    // should tend to propagate
                    case FunctionId.Void:
                    this.variable = SymbolFn.Void;
                    this.ptr = this.trail;
                    break;
                    // in a resolved state, print delay & identity
                    // have a similar effect
                    case FunctionId.Print:
                    output.push(rsv.left.value);
                    case FunctionId.Delay:
                    case FunctionId.Identity:
                    this.variable = rsv.right;
                    this.ptr = this.trail;
                    break;
                    case FunctionId.CallCC:
                    let continuation = this.trail;
                    this.ptr = states.add_resolved(rsv.right, continuation);
                    break;
                    case FunctionId.Constant:
                    this.variable = states.add_constant(rsv.right);
                    this.ptr = this.trail;
                    break;
                    case FunctionId.Substitute:
                    this.variable = states.add_substitute(rsv.right);
                    this.ptr = this.trail;
                    break;
                    case FunctionId.Read:
                    let m = input.next();
                    if (!m.done) {
                        this.current_char = new CharFnNode(FunctionId.Print, m.value);
                        this.ptr = states.add_resolved(rsv.right, SymbolFn.Identity);
                    } else if (input.eof) {
                        this.ptr = states.add_resolved(rsv.right, SymbolFn.Void);
                    } else {
                        input.restart(this);
                        this.exit = true;
                    }
                    break;
                    case FunctionId.Compare:
                    let compare_result;
                    if (!this.current_char || this.current_char.value != rsv.left.value) {
                        compare_result = SymbolFn.Void;
                    } else {
                        compare_result = SymbolFn.Identity;
                    }
                    this.ptr = states.add_resolved(rsv.right, compare_result);
                    break;
                    case FunctionId.Reprint:
                    this.ptr = states.add_resolved(rsv.right, this.current_char);
                    break;
                    // our function is the result of some resolved function
                    case FunctionId.Pointer:
                    switch (rsv.left.type) {
                        case StateType.Identity:
                        let value = states.get_identity(rsv.left);
                        this.ptr = states.add_resolved(value, rsv.right);
                        break;
                        case StateType.Promise:
                        let promise = states.get_promise(rsv.left);
                        this.ptr = states.add_call_left(promise, rsv.right);
                        break;
                        case StateType.Constant:
                        let constant = states.get_constant(rsv.left);
                        this.variable = constant;
                        this.ptr = this.trail;
                        break;
                        case StateType.Substitute:
                        let subst = states.get_substitute(rsv.left);
                        let z = states.add_substitute2(subst, rsv.right);
                        this.variable = z;
                        this.ptr = this.trail;
                        break;
                        case StateType.Substitute2:
                        let subst2 = states.get_substitute2(rsv.left);
                        let left_ptr = states.add_resolved(subst2.left, rsv.right);
                        let right_ptr = states.add_resolved(subst2.right, rsv.right);
                        let left_fn = left_ptr;
                        let right_fn = right_ptr;
                        let replacement = states.add_call_both(left_fn, right_fn);
                        this.ptr = replacement;
                        break;
                        case StateType.Chain:
                        this.variable = rsv.right;
                        this.ptr = rsv.left;
                        break;
                        case StateType.Resolved:
                        throw 'Cannot resolve '+rsv.left+' because we cannot double resolved.';
                        break;
                        case StateType.CallLeft:
                        case StateType.CallRight:
                        case StateType.CallBoth:
                        throw 'Cannot resolve '+rsv.left+' because Calls cannot be resolved.';
                        default:
                        throw "Unknown state type in "+rsv.left;
                    }
                    break;
                    default:
                    throw 'Internal runtime error: '+rsv.left+' is not recognized.'
                }
                break;
                case StateType.Chain:
                let chain = states.get_chain(this.ptr);
                //console.log(''+this.ptr+' '+chain.left+' -> '+chain.right);
                this.trail = chain.right;
                this.ptr = chain.left;
                break;
                case StateType.Identity:
                case StateType.Substitute2:
                case StateType.Constant:
                case StateType.Substitute:
                case StateType.Promise:
                throw "The state "+this.ptr.type+" cannot be evaluated.";
                break;
                default:
                throw "Unknown state type " + this.ptr.type + " in "+this.ptr;
            }
        }
        if (this.ptr.type == StateType.Exit) {
            output.done();
        }
    }
}
