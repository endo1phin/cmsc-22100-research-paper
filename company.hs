-- mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a

import Data.Typeable


cast :: (Typeable a, Typeable b) => a -> Maybe b

(cast ’a’) :: Maybe Char -- Just 'a'
(cast ’a’) :: Maybe Bool -- Nothing


mkT f = case (cast f) of 
    Just g -> g
    Nothing -> id

(mkT not) True -- False
(mkT not) ’a’ -- 'a'


increase :: Float -> Company -> Company
increase k (C ds) = C (map (incD k) ds)
					
incD :: Float -> Dept -> Dept 
incD k (D nm mgr us) = D nm (incE k mgr) (map (incU k) us)
					
incU :: Float -> SubUnit -> SubUnit incU k (PU e) = PU (incE k e)
incU k (DU d) = DU (incD k d)
					
incE :: Float -> Employee -> Employee 
incE k (E p s) = E p (incS k s)
					
incS :: Float -> Salary -> Salary 
incS k (S s) = S (s * (1+k)) 
				
			
		
increase :: Float -> Company -> Company 
increase k = everywhere (mkT (incS k)) 




class Typeable a => Term a where
    gmapT :: (forall b. Term b => b -> b) -> a -> a

-- instance of Employee
instance Term Employee where
    gmapT f (E per sal) = E (f per) (f sal)

-- instance of Bool
instance Term Bool where
    gmapT f x = x

-- schematic code
instance Term C where 
    gmapT f (C t1 ... tn) = C (f t1) ... (f tn)


-- Apply a transformation everywhere, bottom-up
everywhere :: Term a 
    => (forall b. Term b => b -> b) 
    -> a -> a
everywhere f x = f (gmapT (everywhere f) x)

-- 

-- Apply a transformation everywhere, top-down
everywhere’ :: Term a 
    => (forall b. Term b => b -> b) 
    -> a -> a
everywhere’ f x = gmapT (everywhere’ f) (f x)


