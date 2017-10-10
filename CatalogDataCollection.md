# Catalog Data Collection

This document details how the API catalog data are collected from each browser
version. These data are the input for computing [API confluence
metrics](/README.md#the-metrics).

## JavaScript Object Graphs

Dashboard data are based on JavaScript object graphs collected from various
versions of browsers running on actual devices.

### What data are collected

Data are collected by walking the JavaScript object graph, starting from the
global object. Currently, data are collected from a page (i.e., the `window`
object) running on `localhost`. The graph walker uses
`Object.getOwnPropertyNames()` to discover own properties. The properties are
visited breadth first after visiting the object's prototype. The graph walker
stores the following:

1. Type information (object, null, undefined, boolean, number, function);
2. Prototype information;
3. Property descriptor metadata (writable, enumerable, etc.);
4. String representations from `o.toString()` (when available).

### What data are not collected

Plenty of data are missed by this data collection scheme. Examples include:

- Data exposed only in other execution contexts, such as workers;
- APIs that are not exposed in the default global namespace;
  - E.g., `window.getSelection()` returns a `Selection`, which is not a
    constructor that is reachable from the default global namespace
- APIs that are only exposed under special conditions;
  - E.g., Parts of `WebUSB` are not available if not USB devices are available
- APIs that are exposed on instances of objects (not their prototypes), and no
  such objects exist on our test page.

## Lists of APIs

Each browser release is associated with a list of APIs based on an
`Object Graph => API List` algorithm. The algorithm, in general works like this:

1. **Store APIs of libraries**: Look for objects that are properties of the
   global object, such as `window.Math`, and store their own properties;
2. **Store APIs of constructors**: Look for own properties of functions,
   excluding parts of the `Function` API;
3. **Store APIs of `Constructor.prototype`**: Look for `SomeFunction.prototype`
   and store its own properties as APIs of `SomeFunction`;
4. **Store APIs of `Constructor.prototype`'s prototypes**: If
    `SomeFunction.prototype.__proto__` is not `SomeOtherFunction.prototype`,
    then store its own properties as APIs of `SomeFunction` (same with
    `SomeFunction.prototype.__proto__.__proto__`, etc.);
4. **Store APIs of instances**: Some browser versions include important APIs,
   but only expose them on instances of the object. Luckily, such instances tend
   to lurk somewhere on the test page's object graph (e.g.,
   `document.body.style` exposes `CSSStyleDeclaration` on some browser
   versions). Look for objects that not libraries, constructors, or
   `Constructor.prototype`s. Store their own properties as APIs on `Constructor`
   the first time that `instance(.__proto__)^n = Constructor.prototype`.

### Detailed algorithm

In practice, the algorithm relies on a number of heuristics for identifying
libraries, constructors, prototypes, and interface names. The following captures
the overall algorithm, with reference to several heuristics that are described
in more detail below.

```
ExtractApiCatalog(ObjectGraph):

Let fns    := Map: ObjectGraph id -> [Strings]:
                   All objects with a "prototype" property mapped to all names
                   for this object based on "Function naming heuristics"

Let protos := Map: ObjectGraph id -> [ObjectGraph ids]:
                   All objects that are some "Foo.prototype" mapped to all such
                   foos

Let libs   := Map: ObjectGraph id -> [Strings]:
                   All objects that are properties of the global object and
                   are not keys in fns above, mapped to the object's property
                   name on the global objects, plus any class names associated
                   with the object using "Class naming heuristics"

Let apiMap := Map: String -> [Strings]:
                   Map of interface names to their APIs (property and method
                   names); initially empty. By "Store <...> as APIs of" is meant
                   "In apiMap, store API names under every interface name
                   associated with <...>"

For each (fn, names) in fns:
  Store own properties of fn as APIs on each name in names using
  "Class API naming heuristics"

For each (proto, classFns) in protos:
  Let nextProto := proto;
  While nextProto belongs to the same class as proto, based on
  "Prototype class heuristics":
    Store own properties of nextProto as APIs of each classFn in classFns;
    nextProto := Prototype of nextProto

For each (lib, names) in libs:
  Store own properties of lib as APIs on each name in names using
  "Class API naming heuristics"
  Let nextProto := lib;
  While nextProto belongs to the same class as lib, based on
  "Prototype class heuristics":
    Store own properties of nextProto as APIs lib;
    nextProto := Prototype of nextProto

For each object, "instance", that is not a key in fns, protos, or libs:
  Let apis := []
  nextInstance = instance
  While nextInstance is not null, and is not the prototype of a known interface:
    apis := Union of apis and own properties of instance that match
            "Instance API naming heuristics", and do not appear anywhere in
            instance's prototype chain
    nextInstance := Prototype of nextInstance
  If nextInstance is not null, Object.prototype, or Function.prototype:
    Store apis as APIs of nextInstance

For each object, "instance", that is not a key in fns, protos, or libs:
  Store own properties of instance matching "Instance API naming heuristics",
  and do not appear anywhere in instance's prototype chain as APIs of instance

Return apiMap as list of APIs associated with ObjectGraph
```

#### Function naming heuristics

These heuristics are used to derive meaningful names from functions.

An initial list of names for a function is based on the last property name in
all simple paths from the global object to the function. This list is filtered
to exclude meaningless names such as `prototype`, `__proto__`, or
`constructor`. Next, for any path that ends with `Foo.prototype.constructor` for
some `Foo`, `Foo` is added to the list. Finally, any name of a named function
(based on its string representation at object graph collection time) is added to
the list.

#### Class naming heuristics

These heuristics are used to derive meaningful class names for objects
interpreted as class prototypes. These names are derived from the following:

1. The "Function naming heuristics" for the object's own `constructor` property,
   if it exists;
2. `Foo`, for all simple paths from the global object to this object, where the
   path ends with `<...>Foo.prototype`;
3. `Bar`, when the string representation of the object is of the form
   `[object Bar]`, `[object BarPrototype]` or `[object BarConstructor]`.

Note that each of these heuristics can be disabled using configuration
flags. They are all enabled by default.

#### Class API naming heuristics

In general, APIs from a class constructor or prototype include all own property
names, except:

- `Object` APIs, such as `constructor`;
- `Array`-like APIs, such as `0`, `1`, `2`, ...;
- `Function` APIs, such as `prototype`;
- Properties that have a non-`undefined` "value" key in their property
  descriptor
  - Such APIs are regarded as constants, which tend to inflate API counts
    without exposing much interesting information about implementation
    differences

The last of these (the constants) included/excluded with a configuration
flag. Constants are excluded by default.

Of course, `Object.prototype` and `Function.prototype` are treated specially so
that they include their respective APIs.

#### Instance API naming heuristics

In general, APIs from an instance object include all own property names, except:

- `Object` APIs, such as `constructor`;
- `Array`-like APIs, such as `0`, `1`, `2`, ...;

#### Prototype class heuristics

These heuristics are used to walk down the prototype chain of some object and
associate the APIs that are found with the same interface. For example, in some
instances `SomeInterface.prototype.__proto__` contains APIs that belong to
`SomeInterface` because there is no independent interface associated with
`SomeInterface.prototype.__proto__`. The same may apply to `SomeLibrary` and
`SomeLibrary.__proto__`. Such "moving up the prototype chain" continues until
one of the following conditions are met:

1. The next prototype is a global library (e.g., `window.Math`) or an interface
   prototype (e.g., `Document.prototype`);
2. The intersection of names associated with the current interface (using
   "Function naming heuristics") and names associated with the next prototype
   (using "Class naming heuristics") is non-empty.
