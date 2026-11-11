import Lean

/-!
# SHA-256 and the Git blob object id, in Lean

The replay context binds a corpus by the SHA-256 of its raw bytes and records a
Git blob object id as provenance. Both are computed here so the exporter and the
replayer need no external program and no environment: the same bytes give the
same digest anywhere the toolchain runs. The gate recomputes every digest with
`sha256sum` and `git hash-object`, which is what keeps these implementations
honest; the known-answer guards below fail the build on a wrong round constant.
-/

namespace Reactivegas.Digest

private def rotr (x : UInt32) (n : UInt32) : UInt32 := (x >>> n) ||| (x <<< (32 - n))
private def rotl (x : UInt32) (n : UInt32) : UInt32 := (x <<< n) ||| (x >>> (32 - n))

private def k256 : Array UInt32 :=
  #[ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
   , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
   , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
   , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
   , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
   , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
   , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
   , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 ]

/-- Message padding shared by both hashes: `0x80`, zeros to 56 mod 64, then the
bit length as eight big-endian bytes. -/
private def pad (msg : ByteArray) : ByteArray := Id.run do
  let mut out := msg.push 0x80
  while out.size % 64 != 56 do
    out := out.push 0
  let bits : UInt64 := msg.size.toUInt64 * 8
  for i in [0:8] do
    out := out.push ((bits >>> ((7 - i).toUInt64 * 8)).toUInt8)
  return out

private def word (b : ByteArray) (off : Nat) : UInt32 :=
  (b.get! off).toUInt32 <<< 24 ||| (b.get! (off + 1)).toUInt32 <<< 16
    ||| (b.get! (off + 2)).toUInt32 <<< 8 ||| (b.get! (off + 3)).toUInt32

private def hex32 (w : UInt32) : String :=
  let digit := fun (v : UInt32) => Char.ofNat (if v < 10 then 48 + v.toNat else 87 + v.toNat)
  String.mk ((List.range 8).map (fun i => digit ((w >>> ((7 - i).toUInt32 * 4)) &&& 0xf)))

/-- Lowercase hex SHA-256 of the bytes. -/
def sha256 (msg : ByteArray) : String := Id.run do
  let padded := pad msg
  let mut h : Array UInt32 :=
    #[0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]
  for blk in [0:padded.size / 64] do
    let mut w : Array UInt32 := Array.mkEmpty 64
    for t in [0:16] do
      w := w.push (word padded (blk * 64 + t * 4))
    for t in [16:64] do
      let s0 := rotr w[t - 15]! 7 ^^^ rotr w[t - 15]! 18 ^^^ (w[t - 15]! >>> 3)
      let s1 := rotr w[t - 2]! 17 ^^^ rotr w[t - 2]! 19 ^^^ (w[t - 2]! >>> 10)
      w := w.push (w[t - 16]! + s0 + w[t - 7]! + s1)
    let mut a := h[0]!
    let mut b := h[1]!
    let mut c := h[2]!
    let mut d := h[3]!
    let mut e := h[4]!
    let mut f := h[5]!
    let mut g := h[6]!
    let mut hh := h[7]!
    for t in [0:64] do
      let bigS1 := rotr e 6 ^^^ rotr e 11 ^^^ rotr e 25
      let ch := (e &&& f) ^^^ ((~~~e) &&& g)
      let t1 := hh + bigS1 + ch + k256[t]! + w[t]!
      let bigS0 := rotr a 2 ^^^ rotr a 13 ^^^ rotr a 22
      let maj := (a &&& b) ^^^ (a &&& c) ^^^ (b &&& c)
      let t2 := bigS0 + maj
      hh := g
      g := f
      f := e
      e := d + t1
      d := c
      c := b
      b := a
      a := t1 + t2
    h := #[h[0]! + a, h[1]! + b, h[2]! + c, h[3]! + d, h[4]! + e, h[5]! + f, h[6]! + g, h[7]! + hh]
  return String.join (h.toList.map hex32)

/-- Lowercase hex SHA-1 of the bytes (the Git object hash). -/
def sha1 (msg : ByteArray) : String := Id.run do
  let padded := pad msg
  let mut h : Array UInt32 := #[0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0]
  for blk in [0:padded.size / 64] do
    let mut w : Array UInt32 := Array.mkEmpty 80
    for t in [0:16] do
      w := w.push (word padded (blk * 64 + t * 4))
    for t in [16:80] do
      w := w.push (rotl (w[t - 3]! ^^^ w[t - 8]! ^^^ w[t - 14]! ^^^ w[t - 16]!) 1)
    let mut a := h[0]!
    let mut b := h[1]!
    let mut c := h[2]!
    let mut d := h[3]!
    let mut e := h[4]!
    for t in [0:80] do
      let (f, k) :=
        if t < 20 then ((b &&& c) ||| ((~~~b) &&& d), 0x5A827999)
        else if t < 40 then (b ^^^ c ^^^ d, 0x6ED9EBA1)
        else if t < 60 then ((b &&& c) ||| (b &&& d) ||| (c &&& d), 0x8F1BBCDC)
        else (b ^^^ c ^^^ d, 0xCA62C1D6)
      let tmp := rotl a 5 + f + e + k + w[t]!
      e := d
      d := c
      c := rotl b 30
      b := a
      a := tmp
    h := #[h[0]! + a, h[1]! + b, h[2]! + c, h[3]! + d, h[4]! + e]
  return String.join (h.toList.map hex32)

/-- The Git blob object id of a file's bytes: SHA-1 of `blob <length> NUL <bytes>`. -/
def gitBlobOid (content : ByteArray) : String :=
  sha1 ("blob ".toUTF8 ++ (toString content.size).toUTF8 ++ ByteArray.mk #[0] ++ content)

-- Known answers (FIPS 180 test vectors; the Git blob id of the empty file and of
-- "hello\n"): a wrong round constant or rotation fails the build.
#guard sha256 "".toUTF8 == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
#guard sha256 "abc".toUTF8 == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
#guard sha256 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".toUTF8
  == "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
#guard sha1 "abc".toUTF8 == "a9993e364706816aba3e25717850c26c9cd0d89d"
#guard gitBlobOid "".toUTF8 == "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
#guard gitBlobOid "hello\n".toUTF8 == "ce013625030ba8dba906f756967f9e9ca394464a"

end Reactivegas.Digest
