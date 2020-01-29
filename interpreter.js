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
        let current_cache = print_cache;
        let application_cache = new Map ();
        const READ = 0;
        const CHAR = 1;
        const CMMT = 2;
        let state = READ;
        for (let c of source) {
            switch (state) {
                case CMMT:
                if (c == "\n") state = READ;
                break;
                case CHAR:
                char_result = current_cache.get(c);
                if (!char_result) {
                    char_result = new CharFnNode(char_fn, c);
                    current_cache.set(c, char_result);
                }
                state = READ;
                // continue
                case READ:
                let fn = void 8;
                if (char_result) {
                    fn = char_result;
                    char_result = void 8;
                } else {
                    switch (c) {
                        case '`':
                        // defer until we get both arguments
                        left_completed.push(false);
                        break;
                        case '.':
                        char_fn = FunctionId.Print;
                        current_cache = print_cache;
                        state = CHAR;
                        break;
                        case '?':
                        char_fn = FunctionId.Compare;
                        current_cache = compare_cache;
                        state = CHAR;
                        break;
                        case '#':
                        state = CMMT;
                        break;
                        default:
                        fn = char2fn[c];
                        if (fn == null && !c.match(/\s/)) {
                            throw 'Parse error: Unrecognized character '+c+'.'
                        }

                    }
                }
                if (fn != null) {
                    if (left_completed.length == 0) {
                        if (lefts.length > 0) {
                            throw 'Parse error: Unexpected character "'+c+'" after end of program.'+"\n"
                                + 'Are you missing a function application at the beginning?';
                        } else {
                            // program is just a single function, apparently
                            let last_rhs = states.add_resolved(SymbolFn.Identity, fn);
                            lefts.push(last_rhs);
                            last_fn_ptr = last_rhs;
                        }
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
                                states.inc_ref(x);
                                states.inc_ref(y);
                                last_rhs = states.add_application(x, y)
                                subcache.set(y, last_rhs);
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
        if (left_completed.length > 0) {
            throw "Parse error: Source ends before function application is completed."
        }
        states.inc_ref(last_fn_ptr);
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
    rewritables = [];
    add (x) {
        if (this.rewritables.length > 0) {
            let i = this.rewritables.pop();
            this[i] = x;
            return i;
        } else {
            let i = super.push(x)-1;
            this.ref_counts[i] = 0;
            return i;
        }
    }
    inc_ref (i) { return ++this.ref_counts[i] }
    dec_ref (i) {
        let rc = --this.ref_counts[i];
        if (rc == 0) {
            this.rewritables.push(i);
        }
        return rc;
    }
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
    rewritables = [];
    add (x,y) {
        if (this.rewritables.length > 0) {
            let i = this.rewritables.pop();
            this[i] = x;
            this[i+1] = y;
            return i;
        } else {
            let i = super.push(x,y)-2;
            this.ref_counts[i/2] = 0;
            return i;
        }
    }
    inc_ref (i) { return ++this.ref_counts[i/2] }
    dec_ref (i) {
        let rc = --this.ref_counts[i/2];
        if (rc == 0) {
            this.rewritables.push(i);
        }
        return rc;
    }
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
    free_ptrs = [];
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
                case StateType.Chain:
                this.chain_states.inc_ref(x.addr);
                break;
                case StateType.Resolved:
                case StateType.CallLeft:
                case StateType.CallRight:
                case StateType.CallBoth:
                this.call_states.inc_ref(x.addr);
                break;
                case StateType.Substitute2:
                this.substitute_states.inc_ref(x.addr);
                break;
                case StateType.Exit:
                case StateType.Void:
                break;
                default:
                this.simple_states.inc_ref(x.addr);
            }
        }
    }
    dec_ref (x) {
        let dc = 0;
        let to_dec = [x];
        while (to_dec.length > 0) {
            let m = to_dec.pop();
            if (m.id == FunctionId.Pointer) {
                switch (m.type) {
                    case StateType.Chain:
                    if (this.chain_states.dec_ref(m.addr) == 0) {
                        this.free_ptrs.push(m);
                        to_dec.push(this.chain_states[m.addr]);
                        to_dec.push(this.chain_states[m.addr+1]);
                    }
                    break;
                    case StateType.Resolved:
                    case StateType.CallLeft:
                    case StateType.CallRight:
                    case StateType.CallBoth:
                    if (this.call_states.dec_ref(m.addr) == 0) {
                        this.free_ptrs.push(m);
                        to_dec.push(this.call_states[m.addr]);
                        to_dec.push(this.call_states[m.addr+1]);
                    }
                    break;
                    case StateType.Substitute2:
                    if (this.substitute_states.dec_ref(m.addr) == 0) {
                        this.free_ptrs.push(m);
                        to_dec.push(this.substitute_states[m.addr]);
                        to_dec.push(this.substitute_states[m.addr+1]);
                    }
                    break;
                    case StateType.Exit:
                    case StateType.Void:
                    break;
                    default:
                    if (this.simple_states.dec_ref(m.addr) == 0) {
                        this.free_ptrs.push(m);
                        to_dec.push(this.simple_states[m.addr]);
                    }
                }
            }
        }
    }
    add_call (t, x, y) {
        let i = this.call_states.add(x, y);
        if (this.free_ptrs.length > 0) {
            let ptr = this.free_ptrs.pop();
            ptr.type = t;
            ptr.addr = i;
            return ptr;
        } else {
            return new PtrFn(t, i);
        }
    }
    add_call_left (x, y) {
        return this.add_call(StateType.CallLeft, x, y);
    }
    add_call_right (x, y) {
        return this.add_call(StateType.CallRight, x, y);
    }
    add_call_both (x, y) {
        return this.add_call(StateType.CallBoth, x, y);
    }
    get_call ({addr}) {
        return {left: this.call_states[addr], right: this.call_states[addr+1]}
    }
    add_chain (x, y) {
        let i = this.chain_states.add(x, y);
        if (this.free_ptrs.length > 0) {
            let ptr = this.free_ptrs.pop();
            ptr.type = StateType.Chain;
            ptr.addr = i;
            return ptr;
        } else {
            return new PtrFn(StateType.Chain, i);
        }
    }
    get_chain ({addr}) {
        return {left: this.chain_states[addr], right: this.chain_states[addr+1]}
    }
    add_resolved (x, y) {
        return this.add_call(StateType.Resolved, x, y);
    }
    add_simple (t, x) {
        let i = this.simple_states.add(x);
        if (this.free_ptrs.length > 0) {
            let ptr = this.free_ptrs.pop();
            ptr.type = t;
            ptr.addr = i;
            return ptr;
        } else {
            return new PtrFn(t, i)
        }
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
        let i = this.substitute_states.add(x, y);
        return new PtrFn(StateType.Substitute2, i);
    }
    get_substitute2 ({addr}) {
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
            //console.log('P: '+this.ptr+"\nV: "+this.variable+"\nT: "+this.trail+"\n"+states);
            this.steps++;
            switch (this.ptr.type) {
                case StateType.Resolved:
                let rsv = states.get_call(this.ptr);
                //console.log(''+this.ptr+' ['+rsv.left+' '+rsv.right+'] <- '+this.variable);
                if (rsv.left.id == FunctionId.Variable) {
                    states.inc_ref(this.variable);
                    rsv.left = this.variable
                    states.inc_ref(rsv.right);
                } else if (rsv.right.id == FunctionId.Variable) {
                    states.inc_ref(this.variable);
                    rsv.right = this.variable
                    states.inc_ref(rsv.left);
                } else {
                    states.inc_ref(rsv.left);
                    states.inc_ref(rsv.right);
                }
                // the function determines what we do
                switch (rsv.left.id) {
                    // our function is the result of some resolved function
                    case FunctionId.Pointer:
                    switch (rsv.left.type) {
                        case StateType.Substitute:
                        states.dec_ref(this.variable);
                        states.dec_ref(this.ptr);
                        let subst = states.get_substitute(rsv.left);
                        states.inc_ref(subst);
                        states.dec_ref(rsv.left);
                        this.variable = states.add_substitute2(subst, rsv.right);
                        states.inc_ref(this.variable);
                        this.ptr = this.trail;
                        states.inc_ref(this.ptr);
                        break;
                        case StateType.Substitute2:
                        let subst2 = states.get_substitute2(rsv.left);
                        states.inc_ref(subst2.left);
                        states.inc_ref(subst2.right);
                        states.dec_ref(rsv.left);
                        // it would be sufficient to always do: ```sLRV => `LV, `RV, `P[1]P[2].
                        // but it turns out some Ls are very common, and we can optimize
                        // those easily.
                            /* actually im not sure this micro-opting really makes a difference
                        subst_outer: switch (subst2.left.id) {
                            case FunctionId.Identity:
                            // ```siXY = `Y`XY
                            states.inc_ref(rsv.right);
                            states.dec_ref(this.ptr);
                            let iXY = states.add_resolved(subst2.right, rsv.right);
                            states.inc_ref(iXY);
                            this.ptr = states.add_call_right(rsv.right, iXY);
                            states.inc_ref(this.ptr);
                            break;
                            case FunctionId.Substitute:
                            // ```ssXY = ``sY`XY = Z(Y,`XY)
                            states.inc_ref(rsv.right);
                            states.dec_ref(this.ptr);
                            let sY = states.add_substitute(rsv.right);
                            states.inc_ref(sY);
                            let XY = states.add_resolved(subst2.right, rsv.right);
                            states.inc_ref(XY);
                            this.ptr = states.add_call_right(sY, XY, this.ptr);
                            states.inc_ref(this.ptr);
                            break;
                            case FunctionId.Pointer:
                            switch (subst2.left.type) {
                                case StateType.Constant:
                                // ```s`kXYZ = `X`YZ
                                states.dec_ref(this.ptr);
                                let constant = states.get_constant(subst2.left);
                                states.inc_ref(constant);
                                states.dec_ref(subst2.left);
                                let YZ = states.add_resolved(subst2.right, rsv.right);
                                states.inc_ref(YZ);
                                this.ptr = states.add_call_right(constant, YZ);
                                states.inc_ref(this.ptr);
                                break subst_outer;
                                case StateType.Substitute2:
                                // ```s``sXYZW = ```XW`YW`ZW
                                states.inc_ref(rsv.right);
                                states.inc_ref(rsv.right);
                                let subst22 = states.get_substitute2(subst2.left);
                                states.inc_ref(subst22.left);
                                states.inc_ref(subst22.right);
                                states.dec_ref(subst2.left);
                                let XW = states.add_resolved(subst22.left, rsv.right);
                                states.inc_ref(XW);
                                let YW = states.add_resolved(subst22.right, rsv.right);
                                states.inc_ref(YW);
                                let ZW = states.add_resolved(subst2.right, rsv.right);
                                states.inc_ref(ZW);
                                let XWYW = states.add_call_both(XW, YW); // , subst2.left);
                                states.inc_ref(XWYW);
                                this.ptr = states.add_call_both(XWYW, ZW);
                                states.inc_ref(this.ptr);
                                break subst_outer;
                                /* strangely this doesn't work
                                case StateType.Chain:
                                // ```s<cont>XY = `<cont>X
                                this.ptr = states.add_resolved(subst2.left, subst2.right);
                                break subst_outer;
                                /
                                default:
                            }
                            // continue;
                            default:
                            */
                            // ```sXYZ = ``XZ`YZ
                            states.inc_ref(rsv.right);
                            states.dec_ref(this.ptr);
                            let left_ptr = states.add_resolved(subst2.left, rsv.right);
                            let right_ptr = states.add_resolved(subst2.right, rsv.right);
                            states.inc_ref(left_ptr);
                            states.inc_ref(right_ptr);
                            this.ptr = states.add_call_both(left_ptr, right_ptr);
                            states.inc_ref(this.ptr);
                        //}
                        break;
                        case StateType.Chain:
                        states.dec_ref(this.variable);
                        states.dec_ref(this.ptr);
                        this.variable = rsv.right;
                        this.ptr = rsv.left;
                        break;
                        case StateType.Constant:
                        states.dec_ref(this.variable);
                        states.dec_ref(this.ptr);
                        states.dec_ref(rsv.right);
                        let constant = states.get_constant(rsv.left);
                        this.variable = constant;
                        states.inc_ref(this.variable);
                        states.dec_ref(rsv.left);
                        this.ptr = this.trail;
                        states.inc_ref(this.ptr);
                        break;
                        case StateType.Promise:
                        states.dec_ref(this.ptr);
                        let promise = states.get_promise(rsv.left);
                        states.inc_ref(promise);
                        states.dec_ref(rsv.left);
                        this.ptr = states.add_call_left(promise, rsv.right);
                        states.inc_ref(this.ptr);
                        break;
                        case StateType.Void:
                        states.dec_ref(this.variable);
                        states.dec_ref(this.ptr);
                        states.dec_ref(rsv.right);
                        this.variable = SymbolFn.Void;
                        this.ptr = this.trail;
                        states.inc_ref(this.ptr);
                        break;
                        case StateType.Identity:
                        states.dec_ref(this.ptr);
                        let value = states.get_identity(rsv.left);
                        states.inc_ref(value);
                        states.dec_ref(rsv.left);
                        this.ptr = states.add_resolved(value, rsv.right);
                        states.inc_ref(this.ptr);
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
                    case FunctionId.Substitute:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    this.variable = states.add_substitute(rsv.right);
                    states.inc_ref(this.variable);
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case FunctionId.Constant:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    this.variable = states.add_constant(rsv.right);
                    states.inc_ref(this.variable);
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    // in a resolved state, print delay & identity
                    // have a similar effect
                    case FunctionId.Print:
                    output.push(rsv.left.value);
                    case FunctionId.Identity:
                    case FunctionId.Delay:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    this.variable = rsv.right;
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case FunctionId.CallCC:
                    states.dec_ref(this.ptr);
                    let continuation = this.trail;
                    states.inc_ref(this.trail);
                    this.ptr = states.add_resolved(rsv.right, continuation);
                    states.inc_ref(this.ptr);
                    break;
                    case FunctionId.Void:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    states.dec_ref(rsv.right);
                    this.variable = SymbolFn.Void;
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case FunctionId.Read:
                    let m = input.next();
                    if (!m.done) {
                        states.dec_ref(this.ptr);
                        this.current_char = new CharFnNode(FunctionId.Print, m.value);
                        this.ptr = states.add_resolved(rsv.right, SymbolFn.Identity);
                        states.inc_ref(this.ptr);
                    } else if (input.eof) {
                        states.dec_ref(this.ptr);
                        this.ptr = states.add_resolved(rsv.right, SymbolFn.Void);
                        states.inc_ref(this.ptr);
                    } else {
                        states.dec_ref(rsv.right);
                        this.exit = true;
                        input.restart(this);
                    }
                    break;
                    case FunctionId.Compare:
                    let compare_result;
                    if (!this.current_char || this.current_char.value != rsv.left.value) {
                        compare_result = SymbolFn.Void;
                    } else {
                        compare_result = SymbolFn.Identity;
                    }
                    states.dec_ref(this.ptr);
                    this.ptr = states.add_resolved(rsv.right, compare_result);
                    states.inc_ref(this.ptr);
                    break;
                    case FunctionId.Reprint:
                    states.dec_ref(this.ptr);
                    this.ptr = states.add_resolved(rsv.right, this.current_char);
                    states.inc_ref(this.ptr);
                    break;
                    default:
                    throw 'Internal runtime error: '+rsv.left+' is not recognized.'
                }
                break;
                case StateType.Chain:
                let chain = states.get_chain(this.ptr);
                //console.log(''+this.ptr+' '+chain.left+' -> '+chain.right);
                states.inc_ref(chain.right);
                states.inc_ref(chain.left);
                states.dec_ref(this.ptr);
                states.dec_ref(this.trail);
                this.trail = chain.right;
                this.ptr = chain.left;
                break;
                case StateType.CallLeft:
                let call_left = states.get_call(this.ptr);
                states.inc_ref(call_left.left)
                states.inc_ref(call_left.right);
                states.dec_ref(this.ptr);
                this.ptr = call_left.left;
                //console.log(''+this.ptr+' <'+call_left.left+' '+call_left.right+'> <- '+this.variable);
                // adding a chain ensures we come back to <replacement> after we resolve call.left
                let left_replacement= states.add_resolved(SymbolFn.Variable, call_left.right);
                states.inc_ref(left_replacement);
                this.trail = states.add_chain(left_replacement, this.trail);
                states.inc_ref(this.trail);
                break;
                case StateType.CallRight:
                let call_right = states.get_call(this.ptr);
                //console.log(''+this.ptr+' <'+call_right.left+' '+call_right.right+'> <- '+this.variable);
                let call_rl = call_right.left;
                if (call_rl.id == FunctionId.Variable) {
                    states.inc_ref(this.variable);
                    call_rl = this.variable;
                    states.inc_ref(call_right.right);
                } else {
                    states.inc_ref(call_right.left);
                    states.inc_ref(call_right.right);
                }
                if (call_rl.id == FunctionId.Delay) {
                    states.dec_ref(this.ptr);
                    states.dec_ref(this.variable);
                    this.variable = states.add_promise(call_right.right);
                    this.ptr = this.trail;
                    states.inc_ref(this.variable);
                    states.inc_ref(this.trail);
                } else {
                    states.dec_ref(this.ptr);
                    let right_replacement = states.add_resolved(call_rl, SymbolFn.Variable);
                    this.ptr = call_right.right;
                    states.inc_ref(right_replacement);
                    this.trail = states.add_chain(right_replacement, this.trail);
                    states.inc_ref(this.trail);
                }
                break;
                case StateType.CallBoth:
                let call_both = states.get_call(this.ptr);
                states.inc_ref(call_both.left);
                states.inc_ref(call_both.right);
                states.dec_ref(this.ptr);
                //console.log(''+this.ptr+' <'+call_both.left+' '+call_both.right+'> <- '+this.variable);
                let replacement = states.add_call_right(SymbolFn.Variable, call_both.right);
                this.ptr = call_both.left;
                states.inc_ref(replacement);
                this.trail = states.add_chain(replacement, this.trail);
                states.inc_ref(this.trail);
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
