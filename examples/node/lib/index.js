// @flow

const test = "test";

// @genBindings
export const test0: string = "bla";

// @genBindings
export const add = (a: number, b: number) => a + b;

// @genBindings
export const concat = (a: number, b: string) => a + b;

// @genBindings
export const mix = (a: number, b: string, c: string) => a + b + c;
