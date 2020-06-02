pencil:
  1x wood
  -> 1x pencil

sharpen:
  1x pencil
  -> 1x small-pencil
     2x shavings

recycle:
  10x shavings
  -> 1x wood

Base ingredients of recycle:

  1. [10 shavings], [], [], []
    * base ingrs via sharpen
    * production factor 10/2
    * = [(10/2)\*1 pencil], [], [(10/2)*1 small-pencil], [shavings]
  2. [5 pencil], [5 small-pencil], [shavings]
    * base ingrs via pencil
    * production factor 5/1
    * = [5 wood], [], [5 small-pencil], [shavings, pencil]
  3. [5 wood], [5 small-pencil], [shavings, pencil]
    * base ingrs via recycle
    * OH, seen an ingredient (shavings): is a base ingr
    * = [], [wood=5], [5 small-pencil], [shavings, pencil]
  4. Result: wood=5

That makes sense: for every 5 wood you use in creating pencils, you can recover
1.


Base of r1:
  * 1: iC: 4x(2xiB) -- []       -- iC
  * 2: iB: 2x(1xiA) -- []       -- iB
  * 3: iA: 1x(4xiC) -- [3xiD]   -- iA
  * 4: iC: repeat: return 4xiC
