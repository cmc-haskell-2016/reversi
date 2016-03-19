# reversi

[![Build Status](https://travis-ci.org/cmc-haskell-2016/reversi.svg?branch=master)](https://travis-ci.org/cmc-haskell-2016/reversi)

## Установка и запуск

Для установки клонируйте репозиторий и соберите проект с помощью `stack`:

```
git clone https://github.com/cmc-haskell-2016/reversi.git
cd reversi
stack setup
stack build
```

После установки запуск осуществляется командой `stack exec`:

```
stack exec reversi
```

Во время разработки инициировать повторную сборку проекта с последующим запуском рекомендуется
следующей командой:

```
stack build && stack exec reversi
```
