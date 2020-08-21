open import prelude
open import Category
open import Functor
module NatTrans {𝒞 : Cat ℓ1} {𝒟 : Cat ℓ2} where
open import Axiom.Extensionality.Propositional using (Extensionality)

postulate
  extensionality : {A : Set ℓ1} {B : A → Set ℓ2} {f g : (x : A) → B x}
    → (∀ x → f x ≡ g x) → f ≡ g


open Cat 𝒞 renaming (Obj to C; Hom to _↝_ ; id to 𝒾 ; o to _∘_ ;assoc to ∘assoc; idL to 𝒾∘ ; idR to ∘𝒾)
open Cat 𝒟 renaming (Obj to D; Hom to _⇒_ ; id to 𝒿 ; o to _◂_ ;assoc to ◂assoc; idL to 𝒿◂ ; idR to ◂𝒿)
record Nat (ℱ : Fun 𝒞 𝒟) (𝒢 : Fun 𝒞 𝒟) : Set (ℓ1 ⊔ ℓ2) where
    open Fun ℱ renaming (to to F₀; map to F₁; map𝒾 to F𝒾)
    open Fun 𝒢 renaming (to to G₀; map to G₁; map𝒾 to G𝒾)
    field
        to : (x : C) → F₀ x ⇒ G₀ x
        comm : {x y : C} (f : x ↝ y) → to y ◂ F₁ f ≡ G₁ f ◂ to x
module id (ℱ : Fun 𝒞 𝒟) where
  open Fun ℱ renaming (to to F₀; map to F₁; map𝒾 to F𝒾)
  to : (x : C)  → F₀ x ⇒ F₀ x
  to x = F₁ (𝒾 x)
  comm : {x y : C} (f : x ↝ y) → to y ◂ F₁ f ≡ F₁ f ◂ to x
  comm {x} {y} f
                                       = to y ◂ F₁ f
                                      ≡⟨⟩ F₁ (𝒾 y) ◂ F₁ f
     ≡⟨ (λ a → a ◂ F₁ f) ⟨$⟩ F𝒾 y       ⟩ 𝒿 (F₀ y) ◂ F₁ f
     ≡⟨ 𝒿◂ (F₁ f)                       ⟩ F₁ f
     ≡⟨ sym (◂𝒿 (F₁ f))                 ⟩ F₁ f ◂ 𝒿 (F₀ x)
     ≡⟨ (λ a → F₁ f ◂ a) ⟨$⟩ sym (F𝒾 x) ⟩ F₁ f ◂ F₁ (𝒾 x)
                                     ≡⟨⟩ F₁ f ◂ to x ∎
id : (ℱ : Fun 𝒞 𝒟) → Nat ℱ ℱ
id ℱ = record { id ℱ }
module o {ℱ 𝒢 ℋ : Fun 𝒞 𝒟} (n : Nat 𝒢 ℋ) (m : Nat ℱ 𝒢) where
  open Fun ℱ renaming (to to F₀; map to F₁; map𝒾 to F𝒾)
  open Fun 𝒢 renaming (to to G₀; map to G₁; map𝒾 to G𝒾)
  open Fun ℋ renaming (to to H₀; map to H₁; map𝒾 to H𝒾)
  open Nat n      renaming (to to α; comm to αcomm)
  open Nat m      renaming (to to β; comm to βcomm)
  to : (x : C) → F₀ x ⇒ H₀ x
  to x = α x ◂ β x
  comm : {x y : C} (f : x ↝ y) → to y ◂ F₁ f ≡ H₁ f ◂ to x
  comm {x} {y} f
                                      = to y ◂ F₁ f
                                      ≡⟨⟩ (α y ◂ β y) ◂ F₁ f
      ≡⟨ sym (◂assoc (F₁ f) (β y) (α y)) ⟩ α y ◂ (β y ◂ F₁ f)
      ≡⟨ (λ q → α y ◂ q) ⟨$⟩ βcomm f    ⟩ α y ◂ (G₁ f ◂ β x)
      ≡⟨ ◂assoc (β x) (G₁ f) (α y)       ⟩ (α y ◂ G₁ f) ◂ β x
      ≡⟨ (λ a → a ◂ β x) ⟨$⟩ αcomm f ⟩ (H₁ f ◂ α x) ◂ β x
      ≡⟨ sym (◂assoc (β x) (α x) (H₁ f)) ⟩ H₁ f ◂ (α x ◂ β x)
                                      ≡⟨⟩ H₁ f ◂ to x ∎
o : {ℱ 𝒢 ℋ : Fun 𝒞 𝒟} (n : Nat 𝒢 ℋ) (m : Nat ℱ 𝒢) → Nat ℱ ℋ
o n m = record {o n m}
private _∙_ = o
module id-laws {ℱ 𝒢 : Fun 𝒞 𝒟} (n : Nat ℱ 𝒢) where
  open Fun ℱ renaming (to to F₀; map to F₁; map𝒾 to F𝒾)
  open Fun 𝒢 renaming (to to G₀; map to G₁; map𝒾 to G𝒾)
  open Nat n renaming (to to α; comm to αcomm)
  module to where
    idL : Nat.to (id 𝒢 ∙ n) ≡ Nat.to n 
    idL
        = Nat.to (id 𝒢 ∙ n)
        ≡⟨⟩ (λ x → G₁ (𝒾 x) ◂ α x)
        ≡⟨ extensionality (λ x → (λ z → z ◂ α x) ⟨$⟩ G𝒾 x) ⟩ (λ x → 𝒿 (G₀ x) ◂ α x)
        ≡⟨ extensionality (λ x →  𝒿◂ (α x)) ⟩ (λ x → α x)
        ≡⟨⟩ Nat.to n ∎
    idR : Nat.to (n ∙ id ℱ) ≡ Nat.to n
    idR
        = Nat.to (n ∙ id ℱ)
        ≡⟨ extensionality (λ x → (λ z → α x ◂ z) ⟨$⟩ F𝒾 x) ⟩ (λ x → α x ◂ 𝒿 (F₀ x))
        ≡⟨ extensionality (λ x →  ◂𝒿 (α x)) ⟩ Nat.to n ∎
  module comm where
    idL : Nat.comm (id 𝒢 ∙ n) ≡ Nat.comm n 
        -- comm : {x y : C} (f : x ↝ y) → to y ◂ F₁ f ≡ G₁ f ◂ to x
    idL = ?
module assoc-laws {ℱ 𝒢 ℋ ℐ : Fun 𝒞 𝒟} (l : Nat ℱ 𝒢) (m : Nat 𝒢 ℋ) (n : Nat ℋ ℐ) where
  open Fun ℱ renaming (to to F₀; map to F₁; map𝒾 to F𝒾)
  open Fun 𝒢 renaming (to to G₀; map to G₁; map𝒾 to G𝒾)
  open Fun ℋ renaming (to to H₀; map to H₁; map𝒾 to H𝒾)
  open Fun ℐ renaming (to to I₀; map to I₁; map𝒾 to I𝒾)
  open Nat n renaming (to to α; comm to αcomm)
  open Nat m renaming (to to β; comm to βcomm)
  assoc : n ∙ (m ∙ l) ≡ (n ∙ m) ∙ l
  assoc
    = n ∙ (m ∙ l)
    ≡⟨ {!!} ⟩ (n ∙ m) ∙ l ∎
  -- assoc : {a b c d : Obj} (f : a ⇒ b) (g : b ⇒ c) (h : c ⇒ d)
  --     → h ∘ (g ∘ f) ≡ (h ∘ g) ∘ f

  -- extensionality : {A : Set ℓ1} {B : A → Set ℓ2} {f g : (x : A) → B x}
  --   → (∀ x → f x ≡ g x) → f ≡ g

 --     Fioα = {!!}
 -- --   -- β   (Foa (Fi G) n) ≡ α n
 --   -- β   (λ x → o 𝒟 (α (Fi G) x) (α n x)) ≡ α n
 --   -- β   (λ x → o 𝒟 (Fiα G x) (α n x)) ≡ α n
 --   -- β   (λ x → o 𝒟 (F₁ G (id 𝒞 x)) (α n x)) ≡ α n
 --   -- Fid  (λ x → o 𝒟 (id 𝒟 (F₀ G x)) (α n x)) ≡ α n
 --   -- id∘  (λ x → α n x) ≡ α n
 --   --    α n ≡ α n
 --   Fioα {F} {G} n
 -- --    rewrite ExtFid
 --     = {!!}

 -- --  Foα : {F G H : FObj} (n : Nat G H) (m : Nat F G) (x : Obj 𝒞) → Hom 𝒟 (F₀ F x) (F₀ H x)
 --   -- Foα {F} {G} {H} n m x = o 𝒟 (α n x) (α m x)
 --   -- Fiα F x = F₁ F (id 𝒞 x)
 --     -- Fid : (x : Obj 𝒞) → F₁ (id 𝒞 x) ≡ id 𝒟 (F₀ x)


 --   Fio : {F G : FObj} (n : Nat F G) → Fo (Fi G) n ≡ n
 --   Fio {F} {G} n with Fi G | Fo (Fi G) n
 --   ... | record { α = α₁ ; αcomm = αcomm₁ } | record { α = α ; αcomm = αcomm } = {!!}


 -- open FunCat



 -- [_,_] : Cat ℓ1 → Cat ℓ2 → Cat (ℓ1 ⊔ ℓ2)
 -- [ 𝒞 , 𝒟 ] = record
 --               { Obj = FObj 𝒞 𝒟
 --               ; Hom = FHom 𝒞 𝒟
 --               ; id = Fi 𝒞 𝒟
 --               ; o = Fo 𝒞 𝒟
 --               ; id∘ = {! Fio!}
 --               ; ∘id = {!!}
 --               ; ∘assoc = {!!}
 --               }
