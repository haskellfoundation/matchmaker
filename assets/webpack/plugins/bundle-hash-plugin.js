const path = require("path");
const _ = require("lodash");
const fs = require('fs');

const pluginName = "BundleHashPlugin";

class BundleHashPlugin {
  constructor(options) {
    this.options = options;
  }

  apply(compiler) {
    compiler.hooks.compilation.tap(pluginName, (compilation, compilationParams) => {
      console.log(JSON.stringify({compilation}));
      // const assets = compilation.getAssets();
      // console.log({assets})
      // const bundle = _.head(_.filter(assets, (o) => _.endsWith(o.name, ".js")))
      console.log(`Writing the bundle name to ${this.options.publicPath}/bundleName.txt`);
      fs.writeFileSync(`${this.options.publicPath}/bundleName.txt`, bundle.name, "utf8");
    });
  }
}

module.exports = { BundleHashPlugin };
