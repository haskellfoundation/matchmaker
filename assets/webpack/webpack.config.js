"use strict";

const path = require("path");
const { WebpackManifestPlugin } = require("webpack-manifest-plugin");
const HtmlWebpackPlugin = require('html-webpack-plugin')

const postCss = {
  loader: "postcss-loader",
  options: {
    postcssOptions: require("./postcss.config"),
  },
};

const cssLoader = {
  loader: "css-loader",
  options: {
    importLoaders: 1,
  },
};

const publicPath = path.resolve(__dirname, "../../static/");

module.exports = {
  entry: "./js/index.js",
  devtool: "source-map",
  mode: "development",
  output: {
    filename: "[name]-bundle.[contenthash].js",
    path: publicPath,
    clean: true,
  },
  module: {
    rules: [
      {
        test: /\.js$/i,
        exclude: [/node_modules/]
      },
      {
        test: /\.css$/i,
        use: ["style-loader", cssLoader, postCss],
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      filename: "../src/Web/Templates/Layout/layout.jinja",
      template: "./layout.jinja.template",
      inject: false,
    }),
    new WebpackManifestPlugin({
      writeToFileEmit: true,
      publicPath,
    }),
  ]
};
