open import prelude
open import Category

record Fun (𝒞 : Cat ℓ1) (𝒟 : Cat ℓ2) : Set (ℓ1 ⊔ ℓ2) where
    open Cat 𝒞 renaming (Obj to C; Hom to _↝_ ; id to 𝒾 ; o to _∘_ ;assoc to ∘assoc; idL to 𝒾∘ ; idR to ∘𝒾)
    open Cat 𝒟 renaming (Obj to D; Hom to _⇒_ ; id to 𝒿 ; o to _◂_ ;assoc to ◂assoc; idL to 𝒿◂ ; idR to ◂𝒿)
    field
        to : C → D
        map : {x y : C} → x ↝ y → to x ⇒ to y
        map𝒾 : (x : C) → map (𝒾 x) ≡ 𝒿 (to x)
        map∘ : {x y z : C} (f : y ↝ z) (g : x ↝ y) → map (f ∘ g) ≡ map f ◂ map g
