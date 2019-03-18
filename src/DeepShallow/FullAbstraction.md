# Full Abstraction

https://en.wikipedia.org/wiki/Denotational_semantics#Abstraction

For denotational semantics in more intensional models, such as the actor model and process calculi, there are different notions of equivalence within each model, and so the concept of _full abstraction_ is a matter of debate, and harder to pin down. Also the mathematical structure of operational semantics and denotational semantics can become very close.

## Fully Abstract Models of Typed λ-caculi (Milner)

https://www.sciencedirect.com/science/article/pii/0304397577900536

A semantic interpretation `𝒜` for a programming language `L` is fully abstract if, whenever `𝒜⟦𝒞[M]⟧ ⊑ 𝒜⟦𝒞[N]⟧` for two program phrases `M`, `N` and for all program contexts `𝒞[ ]`, it follows that  `𝒜⟦M⟧ ⊑ 𝒜⟦N⟧`. A model `ℳ` for the language is fully abstract if the natural interpretation `𝒜` of `L` in `ℳ` is fully abstract.  
We show that under certain conditions there exists, for an extended typed λ-calculus, a unique fully abstract model.

Now a denotational semantic definition of `L` consists of a semantic domain `D` of meanings, and a semantic interpretation `𝒜 : L → D`. We assume that we are mainly interested in the semantics of (whole) programs. Denote by `𝒞[ ]` a program context - that is, a program with a hole in it, to be filled by a phrase of some kind. One desirable property of `𝒜` is that for all phrases `M` and `N` (of the right kind) we have `𝒜⟦𝒞[M]⟧ = 𝒜⟦𝒞[N]⟧` whenever `𝒜⟦M⟧ = 𝒜⟦N⟧`. This is not hard to achieve, particulary if `𝒜` is given as a homomorphism. But it is unfortunate if for some `M` and `N` such that `𝒜⟦M⟧ ≠ 𝒜⟦N⟧` it nevertheless holds for all program contexts that `𝒜⟦𝒞[M]⟧ = 𝒜⟦𝒞[N]⟧`; it means that `𝒜` distinguishes too finely among nonprogram phrases.

The reason for describing this situation as _over-generous_ is that it typically arises when there are many objects in `D` which cannot be realized (i.e. denoted by a phrase). For example, `𝒜⟦M⟧` and `𝒜⟦N⟧` may be functions which only differ at an unrealizable argument, which can never be supplied to the functions in a program context.  
So we wish to find `D` and `𝒜` such that  

    𝒜⟦M⟧ ⊑ 𝒜⟦N⟧   iff   ∀𝒞[ ]. 𝒜⟦𝒞[M]⟧ ⊑ 𝒜⟦𝒞[N]⟧

(we use `⊑` in place of `=` since we shall always have a partial order over `D`); we call such a semantic definition __fully abstract__.

## Lucius G. Meredith, Introduction to the design of computational calculi 05

https://youtu.be/YH6HuDo3SOU?t=2308

    P ≈₁ Q  ⇔  【P】 ≈₂ 【Q】

_Full Abstraction_ is about behavioral side, not about structural side. Structural equivalence is fundamentally about erasing syntactic differences that don't make a difference and it's not typically used to encode behavior.

Every know process equivalence factors through a form of bisimulation. Even it's much coarser than bisimulation like trace or failure equivalence, they can be turned into a form of bisimulation followed by some form of forgetting or identification. Bisimulation is fundamental or fundational as a notion of equivalence.

**Full Abstraction is parametric in the two different forms of behavioral equivalence.**

In a setting when we pass data structures (and not just names) and we have the data language that is terminating. We can prove that the communication of the data structure would also be terminating.  
A _proof expressions_ can be a means of encoding data structures because we can prove termination and preserve the symmetry of `Par` in the passing data structures. Variables in proof expression terms can be on the left or right-hand side of `Par` so when we reduce, substitutions are on both LHS and RHS.

This led Greg to the SpecialK idea to use Prolog terms. Prolog has symmetry about the unification of the terms. The proof expressions because they come from logic have additional features about resource constraints on the information that's passed between the two. This gives as a sense in which we can embed or combine these kinds of calculi together. We can build variant of the 𝛑-calculus in which the proof expressions of Abramsky's computational model for classical linear logic and it's corresponding reduction relation are sitting at the comm rule for the containing calculus, that uses the proof expressions as representation of data.

## Philip Wadler, Smart Contracts (Plutus Core)

https://youtu.be/IqA-mI2olFA?t=2322

Validator gives complete implementation of the stack to the Redeemer. Is the full abstraction the behavioral equivalence between Plutus terms and stack terms?
  
    P,Q = validator -> redeemer
    ≈₁  = reductions Plutus level
    ≈₂  = reductions stack level
    
    P ≈₁ Q  ⇔  comp P ≈₂ comp Q

```hs
-- validator creates ADT for use by redeemer
validator :: (∀ x. A[x] -> comp B[x]) -> comp C

-- should this be `comp (∀x. A[x] -> comp B[x])`?
redeemer  :: ∀ x. A[x] -> comp B[x]

-- to get `comp B[s]` validator gives stack implementation to redeemer
validator :: (∀ s. s (s->bool)->(num->s->s)->(s->s)->(s->num) -> comp B[s])
          -> comp C
validator redeemer =
  let answer = redeemer stack empty isEmpty push pop top
  in ... do suff with answer ...
```
