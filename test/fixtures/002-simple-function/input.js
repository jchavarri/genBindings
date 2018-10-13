// @flow

// @genBindings
export const add = (a: number, b: number) => a + b;

// $FlowFixMe
const _concat = (a, b) => a + b;

// @genBindings
export const concat = _concat;

// @genBindings
export const mix = (a: number, b: string, c: string) => a + b + c;
