## Домашнее задание №1

Условия выполнения: нужно создать проект через stack, решение должно содержаться в директории `src`. Проект должен собираться через `stack build`. Задачи на редукцию термов, определение слабой головной нормальной формы и подтипов можно написать комментариями, или в любой другой удобной для вас форме.
В файле `stack.yaml` укажите резолвер `14.19`
Дедлайн: 25 февраля

#### 1. Проредуцировать следующие лямбда-термы:

1.1. `((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))`

1.2. `((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]`

#### 2. Реализовать функции по следующим типам.
Нужно реализовать функции согласно ее поведению, описанному в сигнатуре типа данной функции, по которой типы аргументов и значений определяются однозначно, кроме функции `weirdFunction`, где есть два равнозначных правильных ответа. Строго говоря, `undefined` также удовлетворяет данной сигнатуре, имплементация должна быть нетривиальной.

2.1.
```haskell
distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity = undefined
```

2.2.
```haskell
associator
  :: (a, (b, c))
  -> ((a, b), c)
associator = undefined
```

2.3.
```haskell
{-# LANGUAGE TypeOperators #-}

type (<->) a b = (a -> b, b -> a)

eitherAssoc
  :: Either a (Either b c)
  <-> Either (Either a b) c
eitherAssoc = undefined
```

2.4.
```haskell
pairProd
  :: (a -> b)
  -> (c -> d)
  -> (a,c)
  -> (b,d)
pairProd = undefined
```

2.5.
```haskell
weirdFunction
  :: (d -> d -> b)
  -> (a -> b -> c)
  -> (d -> b)
  -> d -> b
weirdFunction = undefined
```

2.6.
```haskell
eitherAssoc
  :: Either a (Either b c)
  -> Either (Either a b) c
eitherAssoc = undefined
```

2.7.
```haskell
distr
  :: (a -> b -> c)
  -> (a -> b)
  -> a -> c
distr = undefined
```

#### 3. Тип натуральных чисел можно определить индуктивно следующим образом

```haskell
data Nat = Zero | Succ Nat
```

3.1. Написать представителя класса типов `Show` для типа натуральных чисел:

```haskell
instance Show Nat where
  show = undefined
```

3.2. Написать представителя класса типов `Eq` для типа `Nat`:

```haskell
instance Eq Nat where
  x == y = undefined
```

3.3. Реализовать инстанс класса типов `Ord` для типа натуральных чисел:
```haskell
instance Ord Nat where
  compare = undefined
```

```haskell
instance Ord Nat where
  x <= y = undefined
```
(Как уже обсуждалось, для реализации представителя класса типов `Ord` достаточно реализовать либо метод `compare`, либо метод `(<=)`. Выберите вариант, какой вам больше нравится)

3.4. Реализовать представителя класса типов `Num` для типа `Nat`
```haskell
instance Num Nat where
  x + y       = undefined
  x * y       = undefined
  x - y       = undefined
  abs         = undefined
  singum      = undefined
  fromInteger = undefined
```
(ясно, что `fromInteger` не может быть тотальной функцией в реализации данного представителя. В случае отрицательного аргумента, переданного данной функции, можно возвращать что-то вроде `error "Natural numbers are nonnegative"`)

3.5. Реализовать представителя класса типов `Enum` для типа `Nat`:
```haskell
instance Enum Nat where
  toEnum   = undefined
  fromEnum = undefined
```

#### 4. В модуле `Data.Function` определена функция `fix`, которая является аналогом комбинатора неподвижной точки:

Реализовать с помощью fix следующие функции:
```haskell
iterateElement :: a -> [a]
iterateElement = undefined
```
Данная функция должна удовлетворять равенству:
```haskell
iterateElement x == [x, x..]
```

```haskell
fibonacci :: Integer -> Integer
fibonacci = undefined

factorial :: Integer -> Integer
factorial = undefined

mapFix :: (a -> b) -> [a] -> [b]
mapFix = undefined
```

#### 5. Определить слабую головную нормальную форму следующих термов:

5.1.
```haskell
distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
```

5.2
```haskell
import Data.Maybe (mapMaybe)

null $ mapMaybe foo "pole chudes ochen' chudesno"
```
где
```haskell
foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True -> Just $ exp pi
      False -> Nothing
```

#### 6. Как вы знаете из лекций, в лямбда-исчислении можно кодировать натуральные числа и арифметические операции над ними.

Определим тип нумералов Черча следующим образом:
```haskell
type Nat a = (a -> a) -> a -> a
```

Определим нуль следующим образом:
```haskell
zero :: Nat a
zero f x = x
```

6.1 Определить функцию прибавления единицы:
```haskell
succChurch :: Nat a -> Nat a
succChurch = undefined
```

6.2-6.3. Реализовать умножение и сложение нумералов Черча:
```haskell
churchPlus, churchMult
  :: Nat a -> Nat a -> Nat a
churchPlus = undefined
churchMult = undefined
```

6.4. Реализовать функцию, которая по нумералу Черча возвращает соответствующее ему целое число:
```haskell
churchToInt :: Nat Integer -> Integer
churchToInt = undefined
```

Функция должна удовлетворять следующим естественным условиям, функция `churchToInt` должна коммутировать с нулем, функцией следования и арифметическими операциями:

a. `churchToInt zero = 0`

b. `churchToInt (succChurch number) = 1 + churchToInt number`

c. `churchToInt (churchPlus m n) = churchToInt m + churchToInt n`

d. `churchToInt (churchMult m n) = churchToInt m * churchToInt n`

#### 7.  Определить типы подтермов в следующих выражениях (включая тип исходного выражения):
В задании требуется предоставить дерево синтаксического разбора (или его подобие) данного терма, каждая вершина которого аннотирована типами.
В терме участвуют полиморфные функции, их типы в требуемом дереве можно инстанировать релевантными типами:

7.1.
```haskell
isUpper . head . head $ map (uncurry id) [((++) "Anna ", "Maks")]
```

7.2
```haskell
(\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
```

7.3.
```haskell
let impl = \x y -> not x || y in
  let isMod2 = \x -> x `mod` 2 == 0 in
  let isMod4 = \x -> x `mod` 4 == 0 in
  \x -> (isMod4 x) `impl` (isMod2 x)
```

#### 8. Определим тип деревьев с двоичным ветвлением

```haskell
data Tree a = Leaf | Node (Tree a) a (Tree a)
```

8.1. Реализовать представителя класса типов `Show` для типа деревьев
```haskell
instance Show a => Show (Tree a) where
  show = undefined
```

8.2. Реализовать представителя класса типов `Eq` для типа деревьев
```haskell
instance Eq a => Eq (Tree a) where
  x == y = undefined
```

8.3. Реализовать функцию, которая возвращает список элементов дерева
```haskell
treeToList :: Tree a -> [a]
treeToList = undefined
```

8.4. Реализовать проверку на пустоту:
```haskell
isEmpty :: Tree a -> Bool
isEmpty = undefined
```

8.5. Реализовать функцию, которая по дереву сопоставляет число его элементов
```haskell
nodesNum :: Tree a -> Int
nodesNum = undefined
```

#### 9. Реализовать следующие функции на списках

9.1. Функция должна повторять каждый элемент столько раз, чему равен сам элемент.
```haskell
ghci> smartReplicate [1,2,3]
[1,2,2,3,3,3]
```

9.2. Напишите функцию, которой передаётся список списков и некоторый элемент. Эта функция должна вернуть список только тех списков, которые содержат переданный элемент.
```haskell
ghci> contains 3 [[1..5], [2,0], [3,4]]
[[1,2,3,4,5],[3,4]]
```

9.3. Требуется написать функцию, которая принимает строку, состоящую только из чисел, разделённых пробельными символами, и находит сумму всех чисел в этой строке. Тип функции должен быть `String -> Int`. В данном задании нужно считать, что переданная строка корректная.

```haskell
ghci> stringSum "1 1"
2
ghci> stringSum "100\n\t-3"
97
```

Тесты ниже:
```haskell
passTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
            , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
            , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
            ]
mustFail  = ["asd", "1-1", "1.2", "--2", "+1", "1+"]
```

9.4. Реализуйте сортировку слиянием:
```haskell
ghci> mergeSort [2, 1, 0, 3, 10, 5]
[0, 1, 2, 3, 5, 10]
```

#### 10. Задачи на типы данных и классы типов

10.1 Рассмотрим тип данных, конструкторы которого обозначают цвета:
```haskell
data Colour = Red | Blue | Purple | Green
  deriving Show
```

Реализовать (частичную функцию), которая по строке возвращает цвет:
```haskell
stringToColour :: String -> Colour
stringToColour = undefined
```

10.2. Рассмотрим тип данных, который вводит различные уровни логгирования:
```haskell
data LogLevel = Error | Warning | Info
  deriving Show
```

Реализовать представителя класса типов `Ord` для типа `LogLevel` через метод `compare` таким образом, чтобы выполнялось соотношение `Error > Warning > Info`:
```haskell
instance Ord LogLevel where
  compare = undefined
```

10.3. Пусть у нас есть запись, которая хранит информацию о человеке: имя, фамилию, возраст и пол
```haskell
data Sex = Male | Female
  deriving (Show, Eq)

data Person
  = Person
  { firstName :: String
  , lastName  :: String
  , age :: Int
  , sex :: Sex
  }
```
Реализуйте функцию `updateLastName person1 person2`, которая меняет фамилию и пол `person2` на фамилию и пол `person1`
```haskell
updateLastName
  :: Person
  -> Person
  -> Person
updateLastName = undefined
```
