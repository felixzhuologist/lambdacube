const lib = import('./lambdacube');

const run = async () => {

const { eval_code } = await lib;
console.log(eval_code("22 * 10 + 3"));

}

run();
