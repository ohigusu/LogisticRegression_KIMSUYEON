本プロジェクトは「Island Scrub Jay」という鳥類の生息確率を予測するために、ロジスティック回帰による空間データ解析を実施した。

## 使用ツール
- 言語: R言語
- モデル: ロジスティック回帰

## データ
- 2008年秋にサンタクルーズ島の307地点で収集された5,625件のデータ

## 目標
- 「Island Scrub Jay」という鳥類の生息確率を予測する

## プロジェクトの流れ
### １．新しい変数の定義
   - Rawデータセットに多項式（Polynomial）や交互作用（Interaction）を追加し、新しい変数を作成する。
### ２．データセット分割
   - TrainデータとTestデータを7:3の割合で分割する。
### ３．分類のThresholdの定義
   - モデルの予測値に対する適切な分類のThresholdを定義する
### ４．最適な説明変数の組み合わせを選択
   - 既存の説明変数と新たに定義した変数の中から、誤分類率（Misclassification Rate）が最小となる組み合わせを探索する。
### 5. 仮説検定
   - 仮説検定を行い、反応変数に有意な影響を与える説明変数を特定する。
### 6. 最適なモデルの可視化

