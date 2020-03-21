## Дополнительное домашнее задание.
#### Дедлайн: 28 марта


#### №1 Параллельный Фибоначчи
Реализовать функцию, возвращающую n-е число Фибоначчи с использованием комбинатора [`rpar`](http://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html#v:rpar), явно указывающего в точке вызова, что стратегия вызова параллельная. Допустима экспоненциальная сложность, функция может многократно пересчитывать значения, а мемоизация не требуется (но и не возбраняется). Весь необходимый функционал содержится в библиотеке `parallel`, смотрите референсы ниже.
```haskell
parFib :: Int -> Int
parFib = undefined
```
(указание: здесь имеет смысл воспользоваться функцией [`runEval`](http://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html#v:runEval))

#### №2 Параллельный map
Функция `pmap` похожа на известную вам функцию `map`, с той разницей, что полученный список вычисляется параллельно, в реализации вам также понадобится комбинатор `rpar`.
```haskell
pmap :: (a -> b) -> [a] -> Eval [b]
pmap = undefined
```
Тест-кейс:

Пусть у нас есть медленный Фибоначчи:
```haskell
slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n - 2)
```

Этот тест должен выполняться не более, чем за одну секунду. Если сомневаетесь, можете увеличить `hugeNumber`.
```haskell
test :: Bool
test
  = let hugeNumber = 30
    in  runEval (pmap slowFib [1, hugeNumber, 5]) !! 2 == 5
```


#### №3 Задача на транзакции с использованием `MVar`
Рассмотрим следующий пример. Пусть у нас есть тип суммы на счете, который является синонимом типа `IORef Integer`. Тип [`IORef`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-IORef.html#t:IORef) - это тип изменяемой переменной в монаде `IO`. Функция `transfer` - это функция, которая пересылает сумму со счета на счет с использованием функций [`readIORef`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-IORef.html#v:readIORef) и [`writeIORef`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-IORef.html#v:readIORef), которые соответственно считывают и записывают значения в изменяемых IO-переменных.

```haskell
type Account = IORef Integer

transfer :: Integer -> Account -> Account -> IO ()
transfer amount from to = do
    fromVal <- readIORef from
    toVal   <- readIORef to
    writeIORef from (fromVal - amount)
    writeIORef to (toVal + amount)
```
Такая реализация трансфера может приводить к “состоянию гонки” - некорректному поведению программы, связанному с порядком вычислений. Например:
1. Есть счёт 1 с суммой 1000р. Клиент выводит с него 500р на счёт 2: `transfer 500 acc1 acc2`
2. Функция выполняет первые две строчки, а runtime вытесняет её в пользу другой. fromVal = 1000.
3. В это время другой клиент отправляет 2000р со счёта 3 на счёт 1. Эта транзакция получает приоритет и полностью выполняется. На счету `acc1` теперь 3000р.
4. Первая транзакция вновь получает приоритет и продолжает выполнение с третьей строки: `writeIORef acc1 (1000 - 500)`, так как в `fromVal` у нас сохранилось значение 1000р.
5. Таким образом счёт 1 потерял 500р, счёт 2 получил 500р, а счёт 3 потерял 2000р, которые исчезли.

Чтобы избежать состояния гонки, используют различные средства синхронизации. [`MVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#t:MVar) - это тип изменяемой переменной, используемой для сообщения между потоками. `MVar` можно мыслить как коробку в более или менее привычном вам смысле. С использованием функций [`takeMVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#v:takeMVar) и [`putMVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#v:putMVar) реализуйте функции `credit`, `debit` и `transfer`.
Функция `credit` считывает значение с текущего счета и прибавляет к нему сумму, кладя результат суммы в изменяемую переменную. Функция `debit` совершает аналогичное действие, только с вычитанием. Тогда функция перевода `transfer` может быть реализована через `credit` и `debit`. Также не допустите deadlock - взаимной блокировки потоков, например:
```haskell
transfer amount from to = do
  …
  fromVal <- takeMVar from
  toVal <- takeMVar to
  ...
```

Такая реализация может вызвать взаимную блокировку, если одновременно будут запрошены транзакции `transfer x acc1 acc2` и `transfer y acc2 acc1`.

```haskell
type Account = MVar Integer

credit :: Integer -> Account -> IO ()
credit amount account = undefined

debit :: Integer -> Account -> IO ()
debit amount account = undefined

transfer :: Integer -> Account -> Account -> IO ()
transfer amount from to = undefined
```

#### №4 Таймаут
Используя потоки, `threadDelay` и исключения как способ прерывания, реализовать функцию:

```haskell
timeout :: Int -> IO a -> IO (Either String a)
timeout = undefined
```

Функция принимает время в микросекундах и IO действие. Если за указанное время действие не выполнится, его нужно прервать и вернуть сообщение об ошибке, иначе результат.
```haskell
timeout 1000000 (pure 1) ~> Right 1
timeout 1000000 (threadDelay 2000000 >> pure 1) ~> Left “Time limit exceeded”
```



#### Ссылки:
[Слайды курса ИТМО по теме](https://slides.com/danielrogozin/lecture-09-concurrency-exceptions-and-parallelism)

[Simon Marlow. Parallel and Concurrent Programming in Haskell](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.468.1136&rep=rep1&type=pdf)

[Simon Peyton Jones, Andrew Gordon, Sigbjorn Finne. Concurrent Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/concurrent-haskell.pdf)

[Библиотека async](https://hackage.haskell.org/package/async)

[Библиотека stm](https://hackage.haskell.org/package/stm)

[Библиотека parallel](https://hackage.haskell.org/package/parallel)
