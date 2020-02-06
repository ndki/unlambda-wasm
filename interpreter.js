// here's an interpreter for Unlambda!
// call Unlambda.parse(<string>) to make a state machine,
// and then call .run() on that to execute it!
export const Unlambda = class {
    // we don't have use for an AST per-se.
    // the language is so simple that its enough
    // to parse by-character, and the only structure
    // of a program is a binary tree of function applications.
    //
    // we use StateData to interpret this tree by directly
    // translating it into a sort-of intermediate format.
    parse (source) {
        let states = new StateData ();
        // since we're going to parse by-character, there are a few
        // states to consider: normal READ mode, where each new character
        // is expected to be a function or an application of functions;
        // CHAR mode, where we complete a function like ".a" or "?z";
        // CMMT mode, where we discard characters after a "#" until EOL.
        const READ = 0;
        const CHAR = 1;
        const CMMT = 2;
        let state = READ;
        // in order to construct our tree in a single pass,
        // we keep track of which part of the tree we're building.
        //
        // last_fn_ptr keeps track of the last branch/node.
        // when two branches have been paired, the resulting node
        // that points to both is stored in last_fn_ptr, and we
        // no longer have to keep track of the pair itself.
        //
        // lefts keeps a list of the un-paired branches.
        //
        // left_completed keeps a list of branch progression,
        // essentially encoding our current location in the
        // binary tree of function applications.  this way,
        // we know when to add a left branch vs when to pair
        // an existing left branch with a new right branch.
        let last_fn_ptr = void 8;
        let lefts = [];
        let left_completed = [];
        // char_fn contains the function type information for
        // either a ".*" or "?*" function.
        let char_fn;
        // caches for the character functions, so ".a.a" refers
        // to the ".a" twice, instead of to two different nodes.
        let print_cache = new Map ();
        let compare_cache = new Map ();
        let current_cache = print_cache;
        // cache for function applications, so "``ii`ii" refers
        // to the "`ii" twice, instead of to two different nodes.
        let application_cache = new Map ();
        // thus we're not truly building a binary tree, but it turns
        // out to be fine. many times compilers desire to un-roll
        // function calls and loops so that the interpreting machine
        // (e.g. the CPU) doesn't have to keep revisiting distant
        // references and re-initializing state. but for Unlambda,
        // this is practically the default execution!! our executable
        // size tends to grow, uh, "exponentially"! as the program
        // un-rolls itself to execute. so being conservative with
        // function creation (and deletion) is actually the main pragma
        // of optimizing Unlambda exection..
        for (let c of source) {
            // current_fn holds the current node, if it exists.
            let current_fn = void 8;
            switch (state) {
                case CMMT:
                // discard until newline
                if (c == "\n") state = READ;
                break;
                case CHAR:
                // complete char_fn as current_fn
                // (possibly cached -- the current_cache is already
                // modified to be the correct one to search)
                current_fn = current_cache.get(c);
                if (!current_fn) {
                    current_fn = new StateFn(char_fn, c);
                    current_cache.set(c, current_fn);
                }
                state = READ;
                // continue to READ execution
                case READ:
                // if we're not coming from CHAR, then we need to
                // figure out what the current character does.
                // if it's a function, we assign to current_fn.
                // if its an incomplete function, the beginning
                // of a comment, or a function application,
                // we change state appropriately and just move on.
                if (!current_fn) {
                    switch (c) {
                        case '`':
                        // this is a function application, so we
                        // defer adding it until we get both arguments,
                        // by incrementing our position on the tree.
                        // TODO handle the case of input after end of program.
                        left_completed.push(false);
                        break;
                        // character functions transition to CHAR:
                        case '.':
                        char_fn = StateType.Print;
                        current_cache = print_cache;
                        state = CHAR;
                        break;
                        case '?':
                        char_fn = StateType.Compare;
                        current_cache = compare_cache;
                        state = CHAR;
                        break;
                        case '#':
                        // start of a comment.
                        state = CMMT;
                        break;
                        default:
                        // the function is well-known, so we can just
                        // look it up. if its whitespace, just skip ahead,
                        // but if its otherwise unrecognized, its an error.
                        current_fn = SymbolFn.fromChar[c];
                        if (current_fn == null && !c.match(/\s/)) {
                            throw 'Parse error: Unrecognized character '+c+'.'
                        }

                    }
                }
                if (current_fn != null) {
                    if (left_completed.length == 0) {
                        // we're at the base of the tree, but still have more
                        // non-trivial input. there are two possibilities:
                        if (lefts.length > 0) {
                            throw 'Parse error: Unexpected character "'+c+'" after end of program.'+"\n"
                                + 'Are you missing a function application at the beginning?';
                        } else {
                            // program is just a single function, apparently
                            let last_rhs = states.add_resolved(SymbolFn.Identity, current_fn);
                            lefts.push(last_rhs);
                            last_fn_ptr = last_rhs;
                        }
                    } else if (left_completed[left_completed.length-1]) {
                        // we already have a left-hand side, so...
                        // complete function application using left and right! :)
                        // then we also need to clean up those function applications
                        // that were waiting on a right-hand side.
                        let last_rhs = current_fn;
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
                        // we don't have a left-hand side yet, so we need
                        // to set this as some left-hand side, and defer until
                        // we get the right-hand side.
                        lefts.push(current_fn);
                        left_completed[left_completed.length-1] = true;
                    }
                }
            }
        }
        if (left_completed.length > 0) {
            // we never reached the base of the tree
            throw "Parse error: Source ends before function application is completed."
        }
        // i should probably explain the reference counting..
        states.inc_ref(last_fn_ptr);
        // anyway states are done, so we can make a StateMachine with them, and
        // use the base call as the entry state.
        let state_machine = new StateMachine (states, last_fn_ptr);
        return state_machine;
    }
}

// we compile to a state machine...  we very frequently add states.
//
// each state represents some "function" application -- although strictly
// speaking, undelimited continuations aren't functions (and for that matter,
// Unlambda functions don't act very much like functions anyway). on the topic,
// even though delimited and undelimited continuations never appear as
// explicit functions in Unlambda code, we use a Continuation state to
// represent the application of a delimited continuation to the
// current variable, (in other words, it represents a node of the callstack),
// and use a Pointer to a Continuation to represent an undelimited continuation.
//
// there are only a few types of function:
// one of the symbol-functions, one of the character-functions,
// the special Variable function, and the special Pointer function.
//
// the Variable special function resolves to the value of 
// the last function application. it is the access point for
// our single internal variable other than "current character".
//
// the Pointer special function is typed according to the state it references.
// so a Pointer to a Constant1 state is a Constant1 Pointer.
// as noted, a Pointer to a Continuation (or Exit) is an undelimited continuation.
// a Pointer to CallX is an unresolved function application and thus has no known value
// -- it has to be "turned into" something more sensible, and its reference evaluated.
// a Pointer to anything else references a state with a well-defined value.
//
// Note: it seems really weird to me that "current character" is
// a "global" variable. i feel like reseting the callstack should
// also reset the "current character" value. but we follow the
// other implementations.

// this is one way to do enums in javascript.
// the class methods only exist on instantiation,
// while the enum map only exists on the class object.
const StateType = class {
    description;
    constructor (description) { this.description = description }
    eval_needed () {
        return this == StateType.CallLeft
            || this == StateType.CallRight
            || this == StateType.CallBoth
            || this == StateType.Resolved;
    }
}
// can also just do StateType.X = Y, but this is a little nicer
let _StateType = {
    // eval-needing functions:
    CallLeft: new StateType('U'), // LHS needs eval
    CallRight: new StateType('V'), // RHS needs eval
    CallBoth: new StateType('W'), // both sides need eval
    Resolved: new StateType('R'), // neither side needs eval
    // stand-in for anything below it:
    Variable: new StateType('X'),
    // execution-changing function:
    Continuation: new StateType('&'),
    // curried functions:
    Identity1: new StateType('I'), // going away please
    Constant1: new StateType('K'),
    Substitute1: new StateType('S'),
    Substitute2: new StateType('Z'),
    Promise: new StateType('D'),
    // simple functions:
    Exit: new StateType('e'),
    Void: new StateType('v'),
    Identity: new StateType('i'),
    Delay: new StateType('d'),
    Constant: new StateType('k'),
    Substitute: new StateType('s'),
    CallCC: new StateType('c'),
    Read: new StateType('@'),
    Reprint: new StateType('|'),
    Pointer: new StateType('P'),
    // char functions:
    Print: new StateType('.'),
    Compare: new StateType('?'),
}
let st_prop = Object.create(null);
st_prop.enumerable = true;
for (let key in _StateType) {
    st_prop.value = _StateType[key];
    Object.defineProperty(StateType, key, st_prop);
}

const StateFn = class {
    type; addr;
    constructor (type, addr) {
        this.type = type;
        this.addr = addr;
    }
    eval_needed () { return this.type.eval_needed() }
    toString () { return this.type.description + this.addr }
}

const SymbolFn = {
    Void: new StateFn(StateType.Void, ''),
    Exit: new StateFn(StateType.Exit, ''),
    Variable: new StateFn(StateType.Variable, ''),
    Identity: new StateFn(StateType.Identity, ''),
    Void: new StateFn(StateType.Void, ''),
    Delay: new StateFn(StateType.Delay, ''),
    Constant: new StateFn(StateType.Constant, ''),
    Substitute: new StateFn(StateType.Substitute, ''),
    CallCC: new StateFn(StateType.CallCC, ''),
    Read: new StateFn(StateType.Read, ''),
    Reprint: new StateFn(StateType.Reprint, ''),
}

SymbolFn.fromChar = {
    i: SymbolFn.Identity,
    v: SymbolFn.Void,
    d: SymbolFn.Delay,
    k: SymbolFn.Constant,
    s: SymbolFn.Substitute,
    c: SymbolFn.CallCC,
    r: new StateFn(StateType.Print, "\n"),
    '@': SymbolFn.Read,
    '|': SymbolFn.Reprint,
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
        if (x.eval_needed()) {
            if (y.eval_needed()) {
                ptr = this.add_call_both(x, y);
            } else {
                ptr = this.add_call_left(x, y);
            }
        } else if (x.type != StateType.Delay && y.eval_needed()) {
            ptr = this.add_call_right(x, y);
        } else {
            // if an application is "side-effect free"ish,
            // and has no pointers, we can (and should) already set up
            // the 'evaluated' state instead of resolving it at runtime
            ptr = this.add_resolved(x, y);
        }
        return ptr;
    }
    inc_ref (x) {
        switch (x.type) {
            case StateType.Continuation:
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
    dec_ref (x) {
        let dc = 0;
        let to_dec = [x];
        while (to_dec.length > 0) {
            let m = to_dec.pop();
            switch (m.type) {
                case StateType.Continuation:
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
    add_call (t, x, y) {
        let i = this.call_states.add(x, y);
        if (this.free_ptrs.length > 0) {
            let ptr = this.free_ptrs.pop();
            ptr.type = t;
            ptr.addr = i;
            return ptr;
        } else {
            return new StateFn(t, i);
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
            ptr.type = StateType.Continuation;
            ptr.addr = i;
            return ptr;
        } else {
            return new StateFn(StateType.Continuation, i);
        }
    }
    get_chain ({addr}) {
        return {left: this.chain_states[addr], right: this.chain_states[addr+1]}
    }
    add_resolved (x, y) {
        switch (x.type) {
            case StateType.Identity:
            return this.add_identity(y);
            break;
            case StateType.Delay:
            return this.add_promise(y);
            break;
            case StateType.Constant:
            return this.add_constant(y);
            break;
            case StateType.Substitute:
            return this.add_substitute(y);
            break;
            case StateType.Void:
            return SymbolFn.Void;
            break;
            case StateType.Exit:
            return SymbolFn.Exit;
            break;
            /// .... ????
            default:
            return this.add_call(StateType.Resolved, x, y);
        }
    }
    add_simple (t, x) {
        let i = this.simple_states.add(x);
        if (this.free_ptrs.length > 0) {
            let ptr = this.free_ptrs.pop();
            ptr.type = t;
            ptr.addr = i;
            return ptr;
        } else {
            return new StateFn(t, i)
        }
    }
    add_constant (x) {
        return this.add_simple(StateType.Constant1, x);
    }
    get_constant ({addr}) {
        return this.simple_states[addr]
    }
    add_identity (x) {
        return this.add_simple(StateType.Identity1, x);
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
        return this.add_simple(StateType.Substitute1, x);
    }
    get_substitute ({addr}) {
        return this.simple_states[addr]
    }
    add_substitute2 (x, y) {
        let i = this.substitute_states.add(x, y);
        return new StateFn(StateType.Substitute2, i);
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
    variable = SymbolFn.Exit;
    current_char = SymbolFn.Void;
    trail = SymbolFn.Exit;
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
            console.log('P: '+this.ptr+"\nV: "+this.variable+"\nT: "+this.trail+"\n"+states);
            this.steps++;
            switch (this.ptr.type) {
                case StateType.Resolved:
                let rsv = states.get_call(this.ptr);
                //console.log(''+this.ptr+' ['+rsv.left+' '+rsv.right+'] <- '+this.variable);
                if (rsv.left.type == StateType.Variable) {
                    states.inc_ref(this.variable);
                    rsv.left = this.variable
                    states.inc_ref(rsv.right);
                } else if (rsv.right.type == StateType.Variable) {
                    states.inc_ref(this.variable);
                    rsv.right = this.variable
                    states.inc_ref(rsv.left);
                } else {
                    states.inc_ref(rsv.left);
                    states.inc_ref(rsv.right);
                }
                // the function determines what we do
                switch (rsv.left.type) {
                    case StateType.Substitute1:
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
                    switch (subst2.left.type) {
                        case StateType.Identity:
                        // ```siXY = `Y`XY
                        states.inc_ref(rsv.right);
                        states.dec_ref(this.ptr);
                        let iXY = states.add_resolved(subst2.right, rsv.right);
                        states.inc_ref(iXY);
                        this.ptr = states.add_call_right(rsv.right, iXY);
                        states.inc_ref(this.ptr);
                        break;
                        case StateType.Substitute:
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
                        case StateType.Constant1:
                        // ```s`kXYZ = `X`YZ
                        states.dec_ref(this.ptr);
                        let constant = states.get_constant(subst2.left);
                        states.inc_ref(constant);
                        states.dec_ref(subst2.left);
                        let YZ = states.add_resolved(subst2.right, rsv.right);
                        states.inc_ref(YZ);
                        this.ptr = states.add_call_right(constant, YZ);
                        states.inc_ref(this.ptr);
                        break;
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
                        break;
                        /* strangely this doesn't work
                        case StateType.Continuation:
                        // ```s<cont>XY = `<cont>X
                        this.ptr = states.add_resolved(subst2.left, subst2.right);
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
                    case StateType.Continuation:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    this.variable = rsv.right;
                    this.ptr = rsv.left;
                    break;
                    case StateType.Constant1:
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
                    case StateType.Identity1:
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
                    break;
                    case StateType.Substitute:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    this.variable = states.add_substitute(rsv.right);
                    states.inc_ref(this.variable);
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Constant:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    this.variable = states.add_constant(rsv.right);
                    states.inc_ref(this.variable);
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    // in a resolved state, print delay & identity
                    // have a similar effect
                    case StateType.Print:
                    output.push(rsv.left.addr);
                    case StateType.Identity:
                    case StateType.Delay:
                    states.dec_ref(this.variable);
                    states.dec_ref(this.ptr);
                    this.variable = rsv.right;
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.CallCC:
                    states.dec_ref(this.ptr);
                    let continuation = this.trail;
                    states.inc_ref(this.trail);
                    this.ptr = states.add_resolved(rsv.right, continuation);
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
                    case StateType.Read:
                    let m = input.next();
                    if (!m.done) {
                        states.dec_ref(this.ptr);
                        this.current_char = new StateFn(StateType.Print, m.value);
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
                    case StateType.Compare:
                    let compare_result;
                    if (!this.current_char || this.current_char.addr != rsv.left.addr) {
                        compare_result = SymbolFn.Void;
                    } else {
                        compare_result = SymbolFn.Identity;
                    }
                    states.dec_ref(this.ptr);
                    this.ptr = states.add_resolved(rsv.right, compare_result);
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Reprint:
                    states.dec_ref(this.ptr);
                    this.ptr = states.add_resolved(rsv.right, this.current_char);
                    states.inc_ref(this.ptr);
                    break;
                    default:
                    throw 'Internal runtime error: '+rsv.left+' is not recognized.'
                }
                break;
                case StateType.Continuation:
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
                console.log(''+this.ptr+' <'+call_right.left+' '+call_right.right+'> <- '+this.variable);
                let call_rl = call_right.left;
                if (call_rl.type == StateType.Variable) {
                    states.inc_ref(this.variable);
                    call_rl = this.variable;
                    states.inc_ref(call_right.right);
                } else {
                    states.inc_ref(call_right.left);
                    states.inc_ref(call_right.right);
                }
                if (call_rl.type == StateType.Delay) {
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
                states.dec_ref(this.variable);
                this.variable = this.ptr;
                this.ptr = this.trail;
                states.inc_ref(this.trail);
                break;
                case StateType.Identity1:
                case StateType.Substitute2:
                case StateType.Constant1:
                case StateType.Substitute1:
                case StateType.Promise:
                states.dec_ref(this.variable);
                this.variable = this.ptr;
                this.ptr = this.trail;
                states.inc_ref(this.trail);
                break;
                case StateType.Variable:
                throw "The state "+this.ptr+" cannot be evaluated.";
                break;
                default:
                throw "Unknown state type " + this.ptr;
            }
        }
        if (this.ptr.type == StateType.Exit) {
            output.done();
        }
    }
}
