module cats where
open import prelude
open import Cat
open import Functor


-- module Cats (𝒞 : Cat ℓ1) (let C = Obj 𝒞; _↝_ = Hom 𝒞; 𝒾 = Cat.id 𝒞; _∘_ = Cat.o 𝒞) 
--            (𝒟 : Cat ℓ2) (let D = Obj 𝒟; _⇒_ = Hom 𝒟; 𝒿 = Cat.id 𝒟; _◂_ = Cat.o 𝒟;
  -- β   (λ x → o 𝒟 (F₁ G (id 𝒞 x)) (α n x)) ≡ α n
  -- Fid  (λ x → o 𝒟 (id 𝒟 (F₀ G x)) (α n x)) ≡ α n

--     FObj = Functor




-- module Product (𝒞 : Cat ℓ1) (𝒟 : Cat ℓ2) where
--     ⊗Obj = Obj 𝒞 × Obj 𝒟
--     ⊗Hom : ⊗Obj → ⊗Obj → _
--     ⊗Hom (x , a) (y , b) = Hom 𝒞 x y × Hom 𝒟 a b
--     ⊗id : ((x , a) : ⊗Obj) → _
--     ⊗id (x , a) = id 𝒞 x , id 𝒟 a
--     ⊗o : {xa yb zc : Obj 𝒞 × Obj 𝒟} → ⊗Hom yb zc → ⊗Hom xa yb → ⊗Hom xa zc
--     ⊗o (f , f') (g , g') = o 𝒞 f g , o 𝒟 f' g'
--     ⊗id∘ : {xa yb : ⊗Obj} (f : ⊗Hom xa yb ) → ⊗o (⊗id yb) f ≡ f
--     ⊗id∘ {xa @ (x , a) } {yb @ (y , b)} (f @ (f₁ , f₂))
--                                                       = ⊗o (⊗id yb) f
--                                                      ≡⟨⟩ o 𝒞 (id 𝒞 y) f₁ , o 𝒟 (id 𝒟 b) f₂
--       ≡⟨ cong ( λ a → o 𝒞 (id 𝒞 y) f₁ , a ) (id∘ 𝒟 f₂) ⟩ o 𝒞 (id 𝒞 y) f₁ , f₂
--       ≡⟨ cong ( λ a → a               , f₂) (id∘ 𝒞 f₁) ⟩ f₁              , f₂
--                                                      ≡⟨⟩ f ∎
--     -- ⊗id∘ (f , f') rewrite id∘ 𝒞 f | id∘ 𝒟 f' = refl
--     ⊗∘id : {xa yb : ⊗Obj} (f : ⊗Hom xa yb ) → ⊗o f (⊗id xa) ≡ f
--     ⊗∘id (f , f') = {!!}
--     -- ⊗∘id (f , f') rewrite ∘id 𝒞 f | ∘id 𝒟 f' = refl
--     ⊗∘assoc : {xa yb zc qd : Obj 𝒞 × Obj 𝒟}
--             (f : ⊗Hom xa yb) (g : ⊗Hom yb zc) (h : ⊗Hom zc qd)
--             → ⊗o h (⊗o g f) ≡ ⊗o (⊗o h g) f
--     ⊗∘assoc (f , f') (g , g') (h , h') = {!!}
-- --     ⊗∘assoc (f , f') (g , g') (h , h') rewrite 𝒞 .∘assoc f  g  h
-- --                                              | 𝒟 .∘assoc f' g' h' = refl
-- _⊗_ : Cat ℓ1 → Cat ℓ2 → Cat (ℓ1 ⊔ ℓ2)
-- 𝒞 ⊗ 𝒟 = record { Product 𝒞 𝒟 renaming (⊗Obj to Obj; ⊗Hom to Hom; ⊗id to id; ⊗o to o
--                                         ; ⊗id∘ to id∘; ⊗∘id to ∘id; ⊗∘assoc to ∘assoc)}
