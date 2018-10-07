# genBindings

Babel plugin to generate Reason bindings based on exported values with the `// @genBindings` annotation.

:warning: Status: Early alpha. Only a few happy paths work at the moment. :warning:

## Requirements

The plugin can be used in projects that are configured with:

- Babel 6
- Flow 0.82

Flow binary (`flow`) is expected to be available in `node_modules/.bin/flow` (no customization via config yet).

## Installation

```sh
$ npm install --save-dev jchavarri/genBindings
```

## Usage

### Via `.babelrc` (Recommended)

**.babelrc**

```json
{
  "plugins": ["genBindings"]
}
```

### Via CLI

```sh
$ babel --plugins genBindings script.js
```

### Via Node API

```javascript
require('babel').transform('code', {
  plugins: ['genBindings']
});
```
