const lookup = dict => key => is_null(dict) ?  undefined :
                              head(head(dict)) === key ? tail(head(dict)) :
                              lookup(tail(dict))(key);
const table = kvpairs => is_null(kvpairs) ? null :
              pair(pair(head(kvpairs), head(tail(kvpairs))), table(tail(tail(kvpairs))));

const flatten = xs => is_null(xs) ? null : !is_list(xs) ? list(xs) :
               append(is_list(head(xs)) ? flatten(head(xs)) : list(head(xs)),
                      flatten(tail(xs)));

const dispatch = list => lookup(table(list));
const map = f => list => is_null(list) ? null : pair(f(head(list)), map(f)(tail(list)));
const flatmap = f => list => flatten(map(f)(list));

const cons = tree => tree(cons);
const snoc = tree => tree(snoc);

const $ = x => is_string(x) ? x : (!is_function(x) || (x(".") === undefined)) ? stringify(x) : x(".");
const scan = x => (!is_function(x) || (x(scan) === undefined)) ? flatten(x) : x(scan);
const foldl = (op, z) => x => (!is_function(x) || (x(foldl) === undefined)) ? op(z,x) : x(foldl)(op,z);
const size = v => (!is_function(v) || (v(size) === undefined)) ? 1 : v(size);

const single = a => dispatch(list(
    ".",  "single(" + $(a) + ")",
    cons, b => tree(digit1(b), empty, digit1(a)),
    snoc, b => tree(digit1(a), empty, digit1(b)),
    scan, scan(a),
    foldl, (op,z) => foldl(op,z)(a),
    size, size(a)
));    

const empty = dispatch(list(
    ".",  "<>",
    cons, single,
    snoc, single,
    scan, null,
    foldl, (op,z) => z,
    size, 0
));

const tree = (left, subtree, right) => dispatch(list(
  ".",  "tree(" + $(left) + "," + $(subtree) + "," + $(right) + ")",
  cons, x => left(cons)(subtree, right)(x),
  snoc, x => right(snoc)(left,subtree)(x),
  scan, flatmap(scan)(list(left, subtree, right)),
  foldl, (op,z) => op(op(op(z,foldl(op,z)(left)),foldl(op,z)(subtree)),foldl(op,z)(right)),
  size, size(left) + size(subtree) + size(right)
));

const digit1 = a => dispatch(list(
  ".", "[" + $(a) + "]",
  cons, (subtree, right) => x => tree(digit2(x,a), subtree, right),
  snoc, (left, subtree) => x => tree(left, subtree, digit2(a,x)),
  scan, flatmap(scan)(list(a)),
  foldl, (op,z) => foldl(op,z)(a),
  size, size(a)
));

const digit2 = (a,b) => dispatch(list(
  ".", "[" + $(a) + "," + $(b) + "]",
  cons, (subtree, right) => x => tree(digit3(x,a,b), subtree, right),
  snoc, (left, subtree) => x => tree(left, subtree, digit3(a,b,x)),
  scan, flatmap(scan)(list(a,b)),
  foldl, (op,z) => op(foldl(op,z)(a),foldl(op,z)(b)),
  size, size(a)+size(b)
));

const digit3 = (a,b,c) => dispatch(list(
  ".", "[" + $(a) + "," + $(b) + "," + $(c) + "]",
  cons,(subtree, right) => x => tree(digit4(x,a,b,c), subtree, right),
  snoc,(left, subtree) => x => tree(left, subtree, digit4(a,b,c,x)),
  scan, flatmap(scan)(list(a,b,c)),
  foldl, (op,z) => op(op(foldl(op,z)(a),foldl(op,z)(b)),foldl(op,z)(c)),
  size, size(a)+size(b)+size(c)
));

const digit4 = (a,b,c,d) => dispatch(list(
  ".", "[" + $(a) + "," + $(b) + "," + $(c) + "," + $(d) + "]",
  cons,(subtree, right) => x => tree(digit2(x,a),subtree(cons)(node3(b,c,d)),right),
  snoc,(left, subtree) => x => tree(left,subtree(snoc)(node3(a,b,c)),digit2(d,x)),
  scan, flatmap(scan)(list(a,b,c,d)),
  foldl, (op,z) => op(op(op(foldl(op,z)(a),foldl(op,z)(b)),foldl(op,z)(c)),foldl(op,z)(d)),
  size, size(a)+size(b)+size(c)+size(d)
));

const node2 = (a,b) => dispatch(list(
  ".",  "(" + $(a) + "," + $(b) + ")",
  scan, flatmap(scan)(list(a,b)),
  foldl, (op,z) => op(op(z,foldl(op,z)(a)),foldl(op,z)(b)),
  size, size(a)+size(b)
));

const node3 = (a,b,c) => dispatch(list(
  ".",  "(" + $(a) + "," + $(b) + "," + $(c) + ")",
  scan, flatmap(scan)(list(a,b,c)),
  foldl, (op,z) => op(op(op(z,foldl(op,z)(a)),foldl(op,z)(b)),foldl(op,z)(c)),
  size, size(a)+size(b)+size(c)
));

const build = n => n === 0 ? empty : build(n-1)(cons)(n);
const t = build(30);
display($(t));
display(t(foldl)((x,y)=>x+y,0));
display(size(t));
"ok";

