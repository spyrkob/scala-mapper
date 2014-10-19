scala-mapper
============

A Scala tool for mapping between POJOs.

### Example mapping:
```scala
map(()=>source.getPerson.getCredentials.getLogin).as(_.toUpperCase).default("Unknown") into target.setLoginName

```
