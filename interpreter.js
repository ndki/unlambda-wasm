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
        let application_cache = new Map ();
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
                        if (!char_result && c == '`') {
                            // defer until we know what we're doing
                            left_completed.push(false);
                        } else if (!char_result && c == '.') {
                            char_fn = FunctionId.Print;
                            state = CHAR;
                        } else if (!char_result && c == '?') {
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
                                    let x = lefts.pop(); let y = last_rhs;
                                    let subcache = application_cache.get(x);
                                    if (!subcache) {
                                        subcache = new Map ();
                                        application_cache.set(x, subcache);
                                    }
                                    last_rhs = subcache.get(y);
                                    if (!last_rhs) {
                                        last_rhs = states.add_application(x, y)
                                        subcache.set(y, last_rhs);
                                    } else {
                                        states.inc_ref(last_rhs);
                                    }
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
    Void: 'V',
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
    constructor (id) { this.id = id }
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
    eval_needed () {
        return this.type == StateType.CallLeft
            || this.type == StateType.CallRight
            || this.type == StateType.CallBoth
            || this.type == StateType.Resolved;
    }
    toString () {
        return this.type+this.addr;
    }
}
const EXIT = new PtrFn(StateType.Exit,0);
const VOID = new PtrFn(StateType.Void,0);

const State = class {
    left; right;
    toString () {
        return this.right ? this.left ? '<'+this.left+' '+this.right+'>'
            : '<'+this.right+'>' : '<>'
    }
}

const RefCounted = class extends Array {
    ref_counts = [];
    push (x) {
        let i = super.push(x);
        this.ref_counts[i-1] = 1;
        return i;
    }
    inc_ref (i) { this.ref_counts[i]++ }
    dec_ref (i) { this.ref_counts[i]-- }
    toString () {
        let s = '';
        for (let i = 0; i < this.length && (s+=' '); i++) {
            s += this[i]+'('+this.ref_counts[i]+')'
        }
        return s;
    }
}

const RefCounted2 = class extends Array {
    ref_counts = [];
    push (x,y) {
        let i = super.push(x,y);
        this.ref_counts[(i-2)/2] = 1;
        return i;
    }
    inc_ref (i) { this.ref_counts[i]++ }
    dec_ref (i) { this.ref_counts[i]-- }
    toString () {
        let s = '';
        for (let i = 0; i < this.length/2 && (s+=' '); i++) {
            s += '<'+this[2*i]+' '+this[2*i+1]+'>('+this.ref_counts[i]+')'
        }
        return s;
    }
}

const StateData = class {
    chain_states = new RefCounted2 ();
    call_states = new RefCounted2 ();
    simple_states = new RefCounted ();
    substitute_states = new RefCounted2 ();
    application_cache = new Map();
    size () {
        return this.call_states.length/2 + this.chain_states.length/2
            + this.simple_states.length + this.substitute_states.length/2
    }
    clone () {
        let sd = new StateData();
        sd.call_states = this.call_states;
        sd.chain_states = this.chain_states;
        sd.simple_states = this.simple_states;
        sd.substitute_states = this.substitute_states;
        return sd;
    }
    add_application (x, y) {
        let ptr;
        if (x.id == FunctionId.Pointer && x.eval_needed()) {
            if (y.id == FunctionId.Pointer && y.eval_needed()) {
                ptr = this.add_call_both(x, y);
            } else {
                ptr = this.add_call_left(x, y);
            }
        } else if (x.id != FunctionId.Delay && y.id == FunctionId.Pointer
            && y.eval_needed()) {
            ptr = this.add_call_right(x, y);
        } else {
            // if an application is "side-effect free"ish,
            // and has no pointers, we can (and should) already set up
            // the 'evaluated' state instead of resolving it at runtime
            switch (x.id) {
                case FunctionId.Identity:
                ptr = this.add_identity(y);
                break;
                case FunctionId.Delay:
                ptr = this.add_promise(y);
                break;
                case FunctionId.Constant:
                ptr = this.add_constant(y);
                break;
                case FunctionId.Substitute:
                ptr = this.add_substitute(y);
                break;
                case FunctionId.Void:
                ptr = VOID;
                break;
                default:
                ptr = this.add_resolved(x, y);
            }
        }
        return ptr;
    }
    inc_ref (x) {
        if (x.id == FunctionId.Pointer) {
            switch (x.type) {
                case StateType.CallLeft:
                case StateType.CallRight:
                case StateType.CallBoth:
                case StateType.Resolved:
                this.call_states.inc_ref(x.addr/2);
                break;
                case StateType.Chain:
                this.chain_states.inc_ref(x.addr/2);
                break;
                case StateType.Substitute2:
                this.substitute_states.inc_ref(x.addr/2);
                break;
                default:
                this.simple_states.inc_ref(x.addr);
            }
        }
    }
    add_call_left (x, y, ptr) {
        if (ptr && this.call_states.ref_counts[ptr.addr/2] == 0) {
            this.call_states[ptr.addr] = x;
            this.call_states[ptr.addr+1] = y;
            ptr.type = StateType.CallLeft;
            return ptr;
        } else {
            let i = this.call_states.push(x, y)-2;
            return new PtrFn(StateType.CallLeft, i);
        }
    }
    add_call_right (x, y, ptr) {
        if (ptr && this.call_states.ref_counts[ptr.addr/2] == 0) {
            this.call_states[ptr.addr] = x;
            this.call_states[ptr.addr+1] = y;
            ptr.type = StateType.CallRight;
            return ptr;
        } else {
            let i = this.call_states.push(x, y)-2;
            return new PtrFn(StateType.CallRight, i);
        }
    }
    add_call_both (x, y, ptr) {
        if (ptr && this.call_states.ref_counts[ptr.addr/2] == 0) {
            this.call_states[ptr.addr] = x;
            this.call_states[ptr.addr+1] = y;
            ptr.type = StateType.CallBoth;
            return ptr;
        } else {
            let i = this.call_states.push(x, y)-2;
            return new PtrFn(StateType.CallBoth, i);
        }
    }
    pop_call ({addr}) {
        this.call_states.dec_ref(addr/2);
        return {left: this.call_states[addr], right: this.call_states[addr+1]}
    }
    add_chain (x, y) {
        let i = this.chain_states.push(x, y)-2;
        return new PtrFn(StateType.Chain, i);
    }
    pop_chain ({addr}) {
        this.chain_states.dec_ref(addr/2);
        return {left: this.chain_states[addr], right: this.chain_states[addr+1]}
    }
    add_resolved (x, y, ptr) {
        if (ptr && this.call_states.ref_counts[ptr.addr/2] == 0) {
            this.call_states[ptr.addr] = x;
            this.call_states[ptr.addr+1] = y;
            this.call_states.inc_ref(ptr.addr/2);
            ptr.type = StateType.Resolved;
            return ptr;
        } else {
            let i = this.call_states.push(x, y)-2;
            return new PtrFn(StateType.Resolved, i);
        }
    }
    pop_resolved ({addr}) {
        this.call_states.dec_ref(addr/2);
        return {left: this.call_states[addr], right: this.call_states[addr+1]}
    }
    add_simple (t, x) {
        let i = this.simple_states.push(x)-1;
        return new PtrFn(t, i)
    }
    add_constant (x) {
        return this.add_simple(StateType.Constant, x);
    }
    pop_constant ({addr}) {
        return this.simple_states[addr]
    }
    add_identity (x) {
        return this.add_simple(StateType.Identity, x);
    }
    pop_identity ({addr}) {
        return this.simple_states[addr]
    }
    add_promise (x) {
        return this.add_simple(StateType.Promise, x);
    }
    pop_promise ({addr}) {
        return this.simple_states[addr]
    }
    add_substitute (x) {
        return this.add_simple(StateType.Substitute, x);
    }
    pop_substitute ({addr}) {
        return this.simple_states[addr]
    }
    add_substitute2 (x, y) {
        let i = this.substitute_states.push(x, y)-2;
        return new PtrFn(StateType.Substitute2, i);
    }
    pop_substitute2 ({addr}) {
        return {left: this.substitute_states[addr], right: this.substitute_states[addr+1]}
    }
    toString () {
        return '&:' + this.chain_states + "\n"+
            'C:' + this.call_states + "\n"+
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
            console.log(''+states);
            this.steps++;
            switch (this.ptr.type) {
                case StateType.CallLeft:
                let call_left = states.pop_call(this.ptr);
                //console.log(''+this.ptr+' <'+call_left.left+' '+call_left.right+'> <- '+this.variable);
                // adding a chain ensures we come back to <replacement> after we resolve call.left
                let left_replacement= states.add_resolved(SymbolFn.Variable, call_left.right, this.ptr);
                this.ptr = call_left.left;
                this.trail = states.add_chain(left_replacement, this.trail);
                break;
                case StateType.CallRight:
                let call_right = states.pop_call(this.ptr);
                //console.log(''+this.ptr+' <'+call_right.left+' '+call_right.right+'> <- '+this.variable);
                let call_rl = call_right.left;
                if (call_rl.id == FunctionId.Variable) { call_rl = this.variable }
                if (call_rl.id == FunctionId.Delay) {
                    states.inc_ref(call_right.right);
                    this.variable = states.add_promise(call_right.right);
                    this.ptr = this.trail;
                } else {
                    let right_replacement = states.add_resolved(call_rl, SymbolFn.Variable, this.ptr);
                    this.ptr = call_right.right;
                    this.trail = states.add_chain(right_replacement, this.trail);
                }
                break;
                case StateType.CallBoth:
                let call_both = states.pop_call(this.ptr);
                //console.log(''+this.ptr+' <'+call_both.left+' '+call_both.right+'> <- '+this.variable);
                let replacement = states.add_call_right(SymbolFn.Variable, call_both.right, this.ptr);
                this.ptr = call_both.left;
                this.trail = states.add_chain(replacement, this.trail);
                break;
                case StateType.Resolved:
                let rsv = states.pop_resolved(this.ptr);
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
                    console.log
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
                        case StateType.Substitute:
                        let subst = states.pop_substitute(rsv.left);
                        let z = states.add_substitute2(subst, rsv.right);
                        this.variable = z;
                        this.ptr = this.trail;
                        break;
                        case StateType.Substitute2:
                        let subst2 = states.pop_substitute2(rsv.left);
                        // it would be sufficient to always do: ```sLRV => `LV, `RV, `P[1]P[2].
                        // but it turns out some Ls are very common, and we can optimize
                        // those easily.
                        subst_outer: switch (subst2.left.id) {
                            case FunctionId.Identity:
                            // ```siXY = `Y`XY
                            states.inc_ref(rsv.right);
                            let iXY = states.add_resolved(subst2.right, rsv.right);
                            this.ptr = states.add_call_right(rsv.right, iXY, this.ptr);
                            break;
                            case FunctionId.Substitute:
                            // ```ssXY = ``sY`XY = Z(Y,`XY)
                            states.inc_ref(rsv.right);
                            let sY = states.add_substitute(rsv.right);
                            let XY = states.add_resolved(subst2.right, rsv.right);
                            this.ptr = states.add_call_right(sY, XY, this.ptr);
                            break;
                            case FunctionId.Pointer:
                            switch (subst2.left.type) {
                                case StateType.Constant:
                                // ```s`kXYZ = `X`YZ
                                let constant = states.pop_constant(subst2.left);
                                let YZ = states.add_resolved(subst2.right, rsv.right);
                                this.ptr = states.add_call_right(constant, YZ, this.ptr);
                                break subst_outer;
                                case StateType.Substitute2:
                                // ```s``sXYZW = ```XW`YW`ZW
                                states.inc_ref(rsv.right);
                                states.inc_ref(rsv.right);
                                let subst22 = states.pop_substitute2(subst2.left);
                                let XW = states.add_resolved(subst22.left, rsv.right);
                                let YW = states.add_resolved(subst22.right, rsv.right);
                                let ZW = states.add_resolved(subst2.right, rsv.right);
                                let XWYW = states.add_call_both(XW, YW); // , subst2.left);
                                this.ptr = states.add_call_both(XWYW, ZW, this.ptr);
                                break subst_outer;
                                /* strangely this doesn't work
                                case StateType.Chain:
                                // ```s<cont>XY = `<cont>X
                                this.ptr = states.add_resolved(subst2.left, subst2.right);
                                break subst_outer;
                                */
                                default:
                            }
                            // continue;
                            default:
                            // ```sXYZ = ``XZ`YZ
                            states.inc_ref(rsv.right);
                            let left_ptr = states.add_resolved(subst2.left, rsv.right);
                            let right_ptr = states.add_resolved(subst2.right, rsv.right);
                            this.ptr = states.add_call_both(left_ptr, right_ptr, this.ptr);
                        }
                        break;
                        case StateType.Chain:
                        this.variable = rsv.right;
                        this.ptr = rsv.left;
                        break;
                        case StateType.Constant:
                        let constant = states.pop_constant(rsv.left);
                        this.variable = constant;
                        this.ptr = this.trail;
                        break;
                        case StateType.Promise:
                        let promise = states.pop_promise(rsv.left);
                        this.ptr = states.add_call_left(promise, rsv.right, this.ptr);
                        break;
                        case StateType.Void:
                        this.variable = SymbolFn.Void;
                        this.ptr = this.trail;
                        break;
                        case StateType.Identity:
                        let value = states.pop_identity(rsv.left);
                        this.ptr = states.add_resolved(value, rsv.right, this.ptr);
                        break;
                        case StateType.Resolved:
                        throw 'Cannot resolve '+rsv.left+' because we cannot double resolve.';
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
                let chain = states.pop_chain(this.ptr);
                //console.log(''+this.ptr+' '+chain.left+' -> '+chain.right);
                this.trail = chain.right;
                this.ptr = chain.left;
                break;
                case StateType.Void:
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
