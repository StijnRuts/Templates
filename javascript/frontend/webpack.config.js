const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  entry: "./src/app.mjs",
  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.ejs",
    }),
  ],
  module: {
    rules: [
      {
        test: /\.(?:js|mjs|cjs)$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            targets: "defaults",
            presets: [["@babel/preset-env"]],
          },
        },
      },
    ],
  },
  output: {
    filename: "index.[contenthash].js",
    path: path.resolve(__dirname, "../public"),
    clean: true,
  },
};
