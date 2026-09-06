import nacl from "tweetnacl";

export const toArrayImpl = (u8) => Array.from(u8);

export const fromArrayImpl = (xs) => new Uint8Array(xs);

export const fromSeedImpl = (seed) => () =>
  nacl.sign.keyPair.fromSeed(seed);
