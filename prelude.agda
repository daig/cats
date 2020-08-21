-- {-# OPTIONS --cubical #-}
module prelude where
import Relation.Binary.PropositionalEquality as Eq
open Eq public using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning public using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Level public using (Level; _⊔_) renaming (zero to lzero; suc to lsuc)
open import Data.Product public using (_×_; proj₁ ; proj₂;  Σ; _,_; ∃; Σ-syntax; ∃-syntax)
-- open import Cubical.Core.Everything
-- open import Cubical.Foundations.Prelude

variable ℓ ℓ1 ℓ2 ℓ3 ℓ4 : Level

_⟨$⟩_ : {A : Set ℓ1} {B : Set ℓ2} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
_⟨$⟩_ = cong
infix 4 _⟨$⟩_
