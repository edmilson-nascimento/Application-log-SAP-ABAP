# ABAP-bal-log

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)
Depois de alguns anos ~~e muita chatice~~ tenho ficado cada vez mais resistente quando alguem me pede para fazer uma _Tabela de Log_ para um _processo X_. Minha resposta sempre é: **você sabia que o SAP tem seu proprio controle de Log sem a necessidade de criar tabelas novas?**
Resolvi fazer minha implementação do [Maravilhos mundo do Application log](https://abapinho.com/2009/09/application-log/). Existem varias utilizações do Application Log (ou mais conhecido como Bal Log), ate mesmo no [wiki scn](https://wiki.scn.sap.com/wiki/display/Snippets/Using+Application+Log) tem alguns exemplos bem exemplicaficados de como utilizar. Para mais exemplos implementados, pode acessar ate na Transação `SE38` com o filtro `SBAL_DEMO_*` que tem uma grande quantidade de aplicações para a solução.

Fiz uma implementação para que pudesse usar a solução das duas formas possiveis:
1- Salvando log de forma que fique acessivel pela transação `SLG1`;
2- Armazenando o log e exbindo ao final do processo sem a necessidade de salvar;

A diferença de implementação é bem simples, então, resolvi fazer de forma a deixar dinamico. Fiz um a classe [zcl_bal_log](https://github.com/edmilson-nascimento/Application-log-SAP-ABAP/blob/master/zcl_bal_log.abap) para melhorar a utilização das funções que contemplam o log. Sendo assim, as utilizações diferente ficam a cargo apenas de usar de forma diferente o método `constructor`, como mostrado nos exemplos abaixo.

## 1- Salvando Log de forma a visualizar pela SLG1 ##
Antes de qualquer coisa, para que os log's sejam salvos, é necessários criar um objeto (caso ainda não tenha sido criado) e um sub-objeto na transação `SLG0`. Esses mesmos objeto e sub-objeto são utilizados para consultor os log's na transação `SLG1`. Não tem nesse exemplo, mas a implementação segue abaixo.

```abap
data:
  app_log type ref to zcl_bal_log.

  create object app_log
    exporting
      title     = 'Titulo do Log'
      object    = 'Z_OBJECT'
      subobject = 'Z_SUBOBJECT'
      alprog    = sy-cprog.
```

## 2- Armazenando em tempo de execução e exibindo ao final do processo ##
Eu uso, em grande parte das vezes, a utilização sem salvar o log. Um dos pontos mais interessantes para a utilização é a forma variada de exibir essa mensagem. Como é mostrado no [post do scn](), tem varias forma que as proprias funções do `Application Log` podem mostrar as mensagens de acordo com a melhor opção para solução. 
Para criação de objeto, deve ser feita da mesma forma que esta no exemplo, conforme modelo abaixo. Ao deixar de informar os valores, a solução ja entende a finalidade de suas mensagens.
```abap
data:
  app_log type ref to zcl_bal_log.

  create object app_log
    exporting
      title     = 'Titulo do Log'
*     object    = 
*     subobject = 
      alprog    = sy-cprog.
```
Simples, objetivo e sem necessidade de ficar criando novas tabelas. :+1:
