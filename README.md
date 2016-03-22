# FORTH

cellは64bit(今回はあまり考えない?)

definitionの詳細はSection 6.2

The dictionary is a linked list.

Linked listの構造は実装依存。複数あるかもしれない(6.6)

re-entrant

space, backspace, carriage retrun以外は名前として使える。

### Dictionary entryの構造

* head
    * link
    * name
    * locate where this word is defined in source code.
    * control bits (type and use of definition)
        * precedence bits, IMMEDIATE による優先順位 コンパイル時にセットされる
        * smudge bit コロン定義中にコンパイラによってセットされ、セミコロンで解除される 循環参照を避ける?
* body 
    * code field(関数ポインタとか、ソースそのままとか)
    * parameter field (実行時に必要になるデータへの参照 variableの値とかが入る)

### STACK

topへのポインタとbottomへのポインタを管理。bottomはアンダーフローの検出やスタックのクリアなど。  


**return stack**

* holds return address for nested definitions.
* holds loop parameters
* other system temp

気をつけて使おう。

### INTERPRETERの動き

入力はエンターを待ってparseされる?

1. スペースをとばしてwordを読み込む
2. dictionaryでwordを検索
    * interpretation modeであればinterpretation behaviorを実行
    * compiling modeであればcompilation behaviorを実行  
   スタックアンダーフローのチェック
    * 平気なら1へ
    * 駄目ならabort
3. wordがdictionaryに無い
    * 数字に変換できれば(interpretation modeなら)スタックに
    * compiling modeでは 実行時にその数字をstackに積む code をcompile(LITERALの定義を見てごらん)
    * 1へ
4. 辞書にもない、数字でもない。 → abort

### VM 

データスタックとリターンスタックをもつ
命令列中のポインタ(Instruction pointer)

------

## 実装についてのメモ

dictionaryは連想リスト
