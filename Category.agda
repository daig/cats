module Category where
open import prelude

record Cat ℓ : Set (lsuc ℓ) where
  field
    Obj : Set ℓ
    Hom : Obj → Obj → Set ℓ
  private _⇒_ = Hom
  field
    id : (x : Obj) → x ⇒ x
    o : {x y z : Obj} → y ⇒ z → x ⇒ y → x ⇒ z
  private _∘_ = o
  field
    idL : {x y : Obj} (f : Hom x y) → id y ∘ f ≡ f
    idR : {x y : Obj} (f : Hom x y) → f ∘ id x ≡ f
    assoc : {a b c d : Obj} (f : a ⇒ b) (g : b ⇒ c) (h : c ⇒ d)
      → h ∘ (g ∘ f) ≡ (h ∘ g) ∘ f

-- open Cat 𝒞 renaming (Obj to C; Hom to _↝_; id to 𝒾; o to _∘_; assoc to ∘assoc; idL to 𝒾∘; idR to ∘𝒾) 


-- open Fun F renaming (to to F₀; map to F₁; map𝒾 to F𝒾)




-- -- natcat : (𝒞 : Cat ℓ1) (𝒟 : Cat ℓ2) → Cat (ℓ1 ⊔ ℓ2)

-- --     Fioα : {ℱ 𝒢 : FObj} (n : Nat ℱ 𝒢) → Nat.to (ι 𝒢 ∙ n) ≡ Nat.to n
-- --     Fioα = {!!}
-- -- --   -- β   (Foa (Fi G) n) ≡ α n
-- --   -- β   (λ x → o 𝒟 (α (Fi G) x) (α n x)) ≡ α n
-- --   -- β   (λ x → o 𝒟 (Fiα G x) (α n x)) ≡ α n
-- --   -- β   (λ x → o 𝒟 (F₁ G (id 𝒞 x)) (α n x)) ≡ α n
-- --   -- Fid  (λ x → o 𝒟 (id 𝒟 (F₀ G x)) (α n x)) ≡ α n
-- --   -- id∘  (λ x → α n x) ≡ α n
-- --   --    α n ≡ α n
-- --   Fioα {F} {G} n
-- -- --    rewrite ExtFid
-- --     = {!!}

-- -- --  Foα : {F G H : FObj} (n : Nat G H) (m : Nat F G) (x : Obj 𝒞) → Hom 𝒟 (F₀ F x) (F₀ H x)
-- --   -- Foα {F} {G} {H} n m x = o 𝒟 (α n x) (α m x)
-- --   -- Fiα F x = F₁ F (id 𝒞 x)
-- --     -- Fid : (x : Obj 𝒞) → F₁ (id 𝒞 x) ≡ id 𝒟 (F₀ x)


-- --   Fio : {F G : FObj} (n : Nat F G) → Fo (Fi G) n ≡ n
-- --   Fio {F} {G} n with Fi G | Fo (Fi G) n
-- --   ... | record { α = α₁ ; αcomm = αcomm₁ } | record { α = α ; αcomm = αcomm } = {!!}


-- -- open FunCat



-- -- [_,_] : Cat ℓ1 → Cat ℓ2 → Cat (ℓ1 ⊔ ℓ2)
-- -- [ 𝒞 , 𝒟 ] = record
-- --               { Obj = FObj 𝒞 𝒟
-- --               ; Hom = FHom 𝒞 𝒟
-- --               ; id = Fi 𝒞 𝒟
-- --               ; o = Fo 𝒞 𝒟
-- --               ; id∘ = {! Fio!}
-- --               ; ∘id = {!!}
-- --               ; ∘assoc = {!!}
-- --               }
