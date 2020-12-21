// 1. closure - semantics of lexical scope
function add1(a) {
    let x = 1;

    let f = () => a + x

    return f
}

// console.log(add1(3)())
// 1) lookup "add" function defined above
// 2) call the function with 3
// 3) return variable f with a maps to 3, x maps to 1 (it is a closure)
// 4) evaluate variable f (evaluate code part in environment part)

// 2. compose
function add2(a) {
    return a + 2
}

function multiply2(factor) {
    return factor * 2
}

function compose(func1, func2) {
    return x => func2(func1(x))
}

let distributive = compose(add2, multiply2)
// console.log(distributive, distributive(4))

// 3. partial application


// 4. curry
/* wrong version
function curry (f, x, y) {
    return f (x, y)
}

let curriedDistributive = curry(compose, add2, multiply2)
*/

let curry = f => y => x => f (x, y)

let curriedDistributive = curry(compose)(multiply2)(add2)
console.log(curriedDistributive(4))

// 5. uncurry
/**
 *  params: curried function
 *  return: general(uncurried) function
 *
 *
 */
function uncurry (func, arguments_of_func) {
    return arguments_of_func => func(arguments_of_func)
}

// 6. callback

// application in redux-thunk