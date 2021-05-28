"use strict";

const path = require("path");
const { WebpackManifestPlugin } = require("webpack-manifest-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin")
const CopyPlugin = require("copy-webpack-plugin");

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
      {
        test: /\.(png|svg|jpg|jpeg|gif)$/i,
        type: "asset/resource",
          generator: {
              filename: "${publicPath}/[name][ext]"
          }
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      filename: "../src/Templates/Layout/layout.html",
      template: "./layout.jinja.template",
      inject: false,
    }),
    new WebpackManifestPlugin({
      writeToFileEmit: true,
      publicPath,
    }),
    new CopyPlugin({
      patterns: [
        { from: "images", to: publicPath },
      ],
    }),
  ]
};
