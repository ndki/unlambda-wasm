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
        // lefts keeps a list of the un-paired branches.
        //
        // left_completed keeps a list of branch progression,
        // essentially encoding our current location in the
        // binary tree of function applications.  this way,
        // we know when to add a left branch vs when to pair
        // an existing left branch with a new right branch.
        let lefts = [];
        let left_completed = [];
        // char_type contains the function type information for
        // either a ".*" or "?*" function.
        let char_type;
        // caches for the character functions, so ".a.a" refers
        // to the ".a" twice, instead of to two different nodes.
        let char_caches = {};
        char_caches[StateType.Print] = new Map ();
        char_caches[StateType.Compare] = new Map ();
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
        for (let index = 0; index < source.length; index++) {
            let character = source[index];
            let current_fn = void 8;
            switch (state) {
		case READ:
                // we need to figure out what the current character does.
                // if it's a function, we assign to current_fn.
                // if its an incomplete function, the beginning
                // of a comment, or a function application,
                // we change state appropriately and just move on.
                switch (character) {
                    case '`':
                    // this is a function application, so we
                    // defer adding it until we get both arguments,
                    // by incrementing our position on the tree.
                    left_completed.push(false);
                    continue;
                    // we were at the base of the tree, but a program
                    // has already been completed!
                    if (left_completed.length == 1 && lefts.length > 0) {
                        throw ParseError.LateInput(source, index, 'function application', character);
                    }
                    break;
                    // normal fn:
                    case 's': current_fn = StateFn.SUBSTITUTE; break;
                    case 'k': current_fn = StateFn.CONSTANT; break;
                    case 'i': current_fn = StateFn.IDENTITY; break;
                    case 'v': current_fn = StateFn.VOID; break;
                    case 'c': current_fn = StateFn.CALLCC; break;
                    case 'd': current_fn = StateFn.DELAY; break;
                    case '@': current_fn = StateFn.READ; break;
                    case '|': current_fn = StateFn.REPRINT; break;
                    case 'r': current_fn = StateFn.NEWLINE; break;
                    // character functions transition to CHAR:
                    case '.':
                    char_type = StateType.Print;
                    state = CHAR;
                    continue;
                    case '?':
                    char_type = StateType.Compare;
                    state = CHAR;
                    continue;
                    // start of a comment:
                    case '#':
                    state = CMMT;
                    continue;
                    default:
                    // if its whitespace, just skip ahead,
                    // but if its otherwise unrecognized, its an error.
                    if (character.match(/\s/)) {
                        continue;
                    } else {
                        throw ParseError.Unrecognized(source, index, character);
                    }
                }
                break;
                case CHAR:
                // complete char_type as current_fn with current char (possibly cached)
                if (char_caches[char_type].has(character)) {
                    current_fn = char_caches[char_type].get(character);
                } else {
                    current_fn = new StateFn(char_type, character);
                    char_caches[char_type].set(character, current_fn);
                }
                state = READ;
                break;
                case CMMT:
                // discard until newline
                if (character == "\n") state = READ;
                continue;
            }
            // if the loop hasn't been <continue>ed then we got a function.
            // check if we're at the base of the tree, but still found a function:
            if (left_completed.length == 0) {
                if (lefts.length > 0) {
                    throw ParseError.LateInput(source, index,
                        current_fn.type.long_description, character);
                }
                // program is just a single function, apparently
                current_fn = states.add_resolved(StateFn.IDENTITY, current_fn);
            }
            // loop as long as we already have an unpaired left-hand side in the tree,
            // and complete function application using left and right! :)
            while (left_completed[left_completed.length-1]) {
                left_completed.pop();
                let left_fn = lefts.pop();
                if (application_cache.has(left_fn)) {
                    let subcache = application_cache.get(left_fn);
                    if (subcache.has(current_fn)) {
                        current_fn = subcache.get(current_fn);
                        continue;
                    }
                } else {
                    application_cache.set(left_fn, new Map ());
                }
                application_cache.get(left_fn).set(current_fn,
                    current_fn = states.add_application(left_fn, current_fn));
            }
            // finally, store whatever our function result is
            lefts.push(current_fn);
            left_completed[left_completed.length-1] = true;
        }
        if (left_completed.length > 0) {
            // we never reached the base of the tree
            throw ParseError.EndsEarly(source, source.length);
        }
        // anyway states are done, so we can make a StateMachine with them, and
        // use the base call as the entry state.
        let state_machine = new StateMachine (states, lefts[0]);
        return state_machine;
    }
}

// more detailed errors are certainly possible,
// but it seems superfluous in this particular case...
const ParseError = class {
    message;
    constructor (source, index, message, hint) {
        this.message = message;
        this.hint = hint;
        let context_len = 15;
        this.source_hint_1 = source.slice(Math.max(index - context_len, 0),index);
        this.source_hint_2 = source.slice(index, index + context_len + 1);
    }
    toString () {
        return "Unlambda ParseError: " +
            this.message + '\n' +
            'Error occurs here: \uFF62' +
                this.source_hint_1 + '<ERROR>' + this.source_hint_2 + '\uFF63\n' +
            this.hint+'\n';
    }
}
Unlambda.ParseError = ParseError;

add_enum(ParseError, {
    EndsEarly: (source, index) => new ParseError
        (source, index, 'Unexpected end of program:\n' +
            'Some function application is still incomplete.',
            'The program may be truncated, or may have too many \uFF62`\uFF63s.'),
    Unrecognized: (source, index, token) => new ParseError
        (source, index, 'Unrecognized token: \uFF62'+token+'\uFF63.',
            'Are you missing a \uFF62.\uFF63, \uFF62?\uFF63 or \uFF62#\uFF63?'),
    LateInput: (source, index, token_type, token) => new ParseError
        (source, index, 'Input after expected end of program:\n' +
            'Got '+token_type+' token \uFF62'+token+'\uFF63 instead of EOF.',
            'Are you missing a function application at the beginning?'),
})

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
    long_description
    constructor (description, long_description) {
        this.description = description;
        this.long_description = long_description;
    }
    eval_needed () {
        switch (this) {
            case StateType.Resolved:
            case StateType.CallLeft:
            case StateType.CallRight:
            case StateType.CallBoth:
            return true;
            default:
            return false;
        }
    }
    is_stored () {
        switch (this) {
            case StateType.Continuation:
            case StateType.Resolved:
            case StateType.CallLeft:
            case StateType.CallRight:
            case StateType.CallBoth:
            case StateType.Substitute2:
            case StateType.Identity1:
            case StateType.Constant1:
            case StateType.Substitute1:
            case StateType.Promise:
            return true;
            default:
            return false;
        }
    }
    is_monad () {
        switch (this) {
            case StateType.Identity1:
            case StateType.Constant1:
            case StateType.Substitute1:
            case StateType.Promise:
            return true;
            default:
            return false;
        }
    }
    is_dyad () {
        switch (this) {
            case StateType.Continuation:
            case StateType.Resolved:
            case StateType.CallLeft:
            case StateType.CallRight:
            case StateType.CallBoth:
            case StateType.Substitute2:
            return true;
            default:
            return false;
        }
    }
    toString () {
        let d = this.long_description;
        return '['+d[0].toUpperCase()+d.slice(1)+']';
    }
}
// can also just do StateType.X = Y, but this is a little nicer
add_enum(StateType, {
    // eval-needing functions:
    CallLeft: new StateType('U', 'call-left'), // LHS needs eval
    CallRight: new StateType('V', 'call-right'), // RHS needs eval
    CallBoth: new StateType('W', 'call-both'), // both sides need eval
    Resolved: new StateType('R', 'resolved'), // neither side needs eval
    // stand-in for anything below it:
    Variable: new StateType('X', 'variable'),
    // execution-changing function:
    Continuation: new StateType('&', 'continuation'),
    // curried functions:
    Identity1: new StateType('I', 'idX'), // going away please
    Constant1: new StateType('K', 'constant'),
    Substitute1: new StateType('S', 'partial substitution combinator'),
    Substitute2: new StateType('Z', 'substitution'),
    Promise: new StateType('D', 'promise'),
    // simple functions:
    Exit: new StateType('e', 'exit'),
    Void: new StateType('v', 'void'),
    Identity: new StateType('i', 'identity'),
    Delay: new StateType('d', 'delay'),
    Constant: new StateType('k', 'constant combinator'),
    Substitute: new StateType('s', 'substitution combinator'),
    CallCC: new StateType('c', 'call/cc'),
    Read: new StateType('@', 'read'),
    Reprint: new StateType('|', 'reprint'),
    // char functions:
    Print: new StateType('.', 'print'),
    Compare: new StateType('?', 'compare'),
});

const StateFn = class {
    type; addr;
    constructor (type, addr) {
        this.type = type;
        this.addr = addr;
    }
    eval_needed () { return this.type.eval_needed() }
    toString () { return this.type.description + this.addr }
}

// some functions can have little a unique StateFn. as a treat.
add_enum(StateFn, {
    VOID: new StateFn(StateType.Void, ''),
    EXIT: new StateFn(StateType.Exit, ''),
    VARIABLE: new StateFn(StateType.Variable, ''),
    IDENTITY: new StateFn(StateType.Identity, ''),
    VOID: new StateFn(StateType.Void, ''),
    DELAY: new StateFn(StateType.Delay, ''),
    CONSTANT: new StateFn(StateType.Constant, ''),
    SUBSTITUTE: new StateFn(StateType.Substitute, ''),
    CALLCC: new StateFn(StateType.CallCC, ''),
    NEWLINE: new StateFn(StateType.Print, "\n"),
    READ: new StateFn(StateType.Read, ''),
    REPRINT: new StateFn(StateType.Reprint, ''),
});

function add_enum (obj, dictionary) {
    let st_prop = Object.create(null);
    st_prop.enumerable = true;
    for (let key in dictionary) {
        st_prop.value = dictionary[key];
        Object.defineProperty(obj, key, st_prop);
    }
}

const StateData = class extends Array {
    free_ptrs = [];
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
            this.ref_counts[i >> 1] = 0;
            return i;
        }
    }
    size () { return this.length >> 1 }
    _inc_ref (i) { return ++this.ref_counts[i >> 1] }
    _dec_ref (i) {
        let rc = --this.ref_counts[i >> 1];
        if (rc == 0) {
            this.rewritables.push(i);
        }
        return rc;
    }
    inc_ref (x) {
        if (x.type.is_stored()) {
            this._inc_ref(x.addr);
        }
    }
    dec_ref (x) {
        let dc = 0;
        let to_dec = [x];
        while (to_dec.length > 0) {
            let m = to_dec.pop();
            if (m.type.is_monad()) {
                if (this._dec_ref(m.addr) == 0) {
                    this.free_ptrs.push(m);
                    to_dec.push(this[m.addr]);
                }
            } else if (m.type.is_dyad()) {
                if (this._dec_ref(m.addr) == 0) {
                    this.free_ptrs.push(m);
                    to_dec.push(this[m.addr]);
                    to_dec.push(this[m.addr+1]);
                }
            /*
            } else {
                throw 'Internal runtime error: Tried to decrease reference count at '+m+
                    ' but '+m.type+' is not a storable type';
            */
            }
        }
    }
    add_application (x, y) {
        this.inc_ref(x);
        this.inc_ref(y);
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
            // and makes no calls, we can (and should) already set up
            // the 'evaluated' state instead of resolving it at runtime
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
                /* its not clear why this doesn't work
                case StateType.Substitute1:
                let s = this.get_monad(x);
                this.dec_ref(x);
                return this.add_substitute(s, y);
                break;
                */
                case StateType.Void:
                return StateFn.VOID;
                break;
                case StateType.Exit:
                return StateFn.EXIT;
                break;
                default:
                ptr = this.add_resolved(x, y);
            }
        }
        return ptr;
    }
    add_monad (t, x) {
        let i = this.add(StateFn.IDENTITY, x);
        if (this.free_ptrs.length > 0) {
            let ptr = this.free_ptrs.pop();
            ptr.type = t;
            ptr.addr = i;
            return ptr;
        } else {
            return new StateFn(t, i)
        }
    }
    add_dyad (t, x, y) {
        let i = this.add(x, y);
        if (this.free_ptrs.length > 0) {
            let ptr = this.free_ptrs.pop();
            ptr.type = t;
            ptr.addr = i;
            return ptr;
        } else {
            return new StateFn(t, i);
        }
    }
    get_monad ({addr}) { return this[addr + 1] }
    get_dyad ({addr}) { return {left: this[addr], right: this[addr+1]} }
    add_constant (x) { return this.add_monad(StateType.Constant1, x) }
    add_identity (x) { return this.add_monad(StateType.Identity1, x) }
    add_promise (x) { return this.add_monad(StateType.Promise, x) }
    add_substitute (x) { return this.add_monad(StateType.Substitute1, x) }
    add_call_left (x, y) { return this.add_dyad(StateType.CallLeft, x, y) }
    add_call_right (x, y) { return this.add_dyad(StateType.CallRight, x, y) }
    add_call_both (x, y) { return this.add_dyad(StateType.CallBoth, x, y) }
    add_chain (x, y) { return this.add_dyad(StateType.Continuation, x, y) }
    add_resolved (x, y) { return this.add_dyad(StateType.Resolved, x, y) }
    add_substitute2 (x, y) { return this.add_dyad(StateType.Substitute2, x, y) }
    toString () {
        let s = '';
        for (let i = 0; i < this.length; i += 2) {
            s += i+': <'+this[i]+' '+this[i+1]+'>('+this.ref_counts[i >> 1]+')\n'
        }
        return s;
    }
}

const StateMachine = class {
    _variable = StateFn.EXIT;
    get variable () {
        return this._variable;
    }
    get dup_variable () {
        this.states.inc_ref(this._variable);
        return this._variable;
    }
    set overwrite_variable (new_value) {
        this.states.dec_ref(this._variable);
        return this._variable = new_value;
    }
    current_char = StateFn.VOID;
    trail = StateFn.EXIT;
    ptr; states;
    exit = false;
    steps = 0;
    constructor (states, init) {
        this.states = states;
        this.states.inc_ref(init);
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
                let rsv = states.get_dyad(this.ptr);
                //console.log(''+this.ptr+' ['+rsv.left+' '+rsv.right+'] <- '+this.variable);
                if (rsv.left.type == StateType.Variable) {
                    rsv.left = this.dup_variable;
                    states.inc_ref(rsv.right);
                } else if (rsv.right.type == StateType.Variable) {
                    rsv.right = this.dup_variable;
                    states.inc_ref(rsv.left);
                } else {
                    states.inc_ref(rsv.left);
                    states.inc_ref(rsv.right);
                }
                // the function determines what we do
                switch (rsv.left.type) {
                    case StateType.Substitute1:
                    states.dec_ref(this.ptr);
                    let subst = states.get_monad(rsv.left);
                    states.inc_ref(subst);
                    states.dec_ref(rsv.left);
                    this.overwrite_variable = states.add_substitute2(subst, rsv.right);
                    states.inc_ref(this.variable);
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Substitute2:
                    let subst2 = states.get_dyad(rsv.left);
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
                        let constant = states.get_monad(subst2.left);
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
                        let subst22 = states.get_dyad(subst2.left);
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
                    states.dec_ref(this.ptr);
                    this.overwrite_variable = rsv.right;
                    this.ptr = rsv.left;
                    break;
                    case StateType.Constant1:
                    states.dec_ref(this.ptr);
                    states.dec_ref(rsv.right);
                    let constant = states.get_monad(rsv.left);
                    this.overwrite_variable = constant;
                    states.inc_ref(this.variable);
                    states.dec_ref(rsv.left);
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Promise:
                    states.dec_ref(this.ptr);
                    let promise = states.get_monad(rsv.left);
                    states.inc_ref(promise);
                    states.dec_ref(rsv.left);
                    this.ptr = states.add_call_left(promise, rsv.right);
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Void:
                    states.dec_ref(this.ptr);
                    states.dec_ref(rsv.right);
                    this.overwrite_variable = StateFn.VOID;
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Identity1:
                    states.dec_ref(this.ptr);
                    let value = states.get_monad(rsv.left);
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
                    states.dec_ref(this.ptr);
                    this.overwrite_variable = states.add_substitute(rsv.right);
                    states.inc_ref(this.variable);
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Constant:
                    states.dec_ref(this.ptr);
                    this.overwrite_variable = states.add_constant(rsv.right);
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
                    states.dec_ref(this.ptr);
                    this.overwrite_variable = rsv.right;
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
                    states.dec_ref(this.ptr);
                    states.dec_ref(rsv.right);
                    this.overwrite_variable = StateFn.VOID;
                    this.ptr = this.trail;
                    states.inc_ref(this.ptr);
                    break;
                    case StateType.Read:
                    let m = input.next();
                    if (!m.done) {
                        states.dec_ref(this.ptr);
                        this.current_char = new StateFn(StateType.Print, m.value);
                        this.ptr = states.add_resolved(rsv.right, StateFn.IDENTITY);
                        states.inc_ref(this.ptr);
                    } else if (input.eof) {
                        states.dec_ref(this.ptr);
                        this.ptr = states.add_resolved(rsv.right, StateFn.VOID);
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
                        compare_result = StateFn.VOID;
                    } else {
                        compare_result = StateFn.IDENTITY;
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
                let chain = states.get_dyad(this.ptr);
                //console.log(''+this.ptr+' '+chain.left+' -> '+chain.right);
                states.inc_ref(chain.right);
                states.inc_ref(chain.left);
                states.dec_ref(this.ptr);
                states.dec_ref(this.trail);
                this.trail = chain.right;
                this.ptr = chain.left;
                break;
                case StateType.CallLeft:
                let call_left = states.get_dyad(this.ptr);
                states.inc_ref(call_left.left)
                states.inc_ref(call_left.right);
                states.dec_ref(this.ptr);
                this.ptr = call_left.left;
                //console.log(''+this.ptr+' <'+call_left.left+' '+call_left.right+'> <- '+this.variable);
                // adding a chain ensures we come back to <replacement> after we resolve call.left
                let left_replacement= states.add_resolved(StateFn.VARIABLE, call_left.right);
                states.inc_ref(left_replacement);
                this.trail = states.add_chain(left_replacement, this.trail);
                states.inc_ref(this.trail);
                break;
                case StateType.CallRight:
                let call_right = states.get_dyad(this.ptr);
                //console.log(''+this.ptr+' <'+call_right.left+' '+call_right.right+'> <- '+this.variable);
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
                    this.overwrite_variable = states.add_promise(call_right.right);
                    this.ptr = this.trail;
                    states.inc_ref(this.variable);
                    states.inc_ref(this.trail);
                } else {
                    states.dec_ref(this.ptr);
                    let right_replacement = states.add_resolved(call_rl, StateFn.VARIABLE);
                    this.ptr = call_right.right;
                    states.inc_ref(right_replacement);
                    this.trail = states.add_chain(right_replacement, this.trail);
                    states.inc_ref(this.trail);
                }
                break;
                case StateType.CallBoth:
                let call_both = states.get_dyad(this.ptr);
                states.inc_ref(call_both.left);
                states.inc_ref(call_both.right);
                states.dec_ref(this.ptr);
                //console.log(''+this.ptr+' <'+call_both.left+' '+call_both.right+'> <- '+this.variable);
                let replacement = states.add_call_right(StateFn.VARIABLE, call_both.right);
                this.ptr = call_both.left;
                states.inc_ref(replacement);
                this.trail = states.add_chain(replacement, this.trail);
                states.inc_ref(this.trail);
                break;
                case StateType.Void:
                this.overwrite_variable = this.ptr;
                this.ptr = this.trail;
                states.inc_ref(this.trail);
                break;
                case StateType.Identity1:
                case StateType.Substitute2:
                case StateType.Constant1:
                case StateType.Substitute1:
                case StateType.Promise:
                this.overwrite_variable = this.ptr;
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
