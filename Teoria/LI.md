# Apunts LI

## Avaluació

- T 60%

  - Examen parcial de Lògica Proposicional (30 abril)
  - Examen Final (conta el màxim)
    - L.Prop
    - L.1r Orden

- L 40%

  - 2 ex de Laboratorio


## Día 1. Teoría

```
los que saben, no hablan : s -> ¬h = ¬s V ¬h
los que hablan, no saben : h -> ¬h = ¬h V ¬s
```

### Lógica

- Sintaxis

  -  ¿Qué es una fórmula F? Una fórmula es una expresión.

- Semántica (significado)
  - ¿Qué es una interpretación I?
  - ¿Cuándo una I satisface una F? 

    - I ![\vdash ](https://wikimedia.org/api/rest_v1/media/math/render/svg/a0c0d30cf8cb7dba179e317fcde9583d842e80f6)F, es decir I es un modelo de F


  ### Definiciones

- F satisfactible, que F tiene algun modelo

- F insatisfactible, que F no tiene ningun modelo

- F tautología, todas las interpretaciones son modelos

- F ![\vdash ](https://wikimedia.org/api/rest_v1/media/math/render/svg/a0c0d30cf8cb7dba179e317fcde9583d842e80f6)G (G consecuencia lógica de F), todos los modelos de F son modelos de G

- F ≡ G (F y G son lógicamente equivalentes), toda interpretación que satisface F también satisface a G es decir tienen los mismos modelos 

- G es consecuencia lógica de F  ssi F ∧ G insat

- F y G son lógicamente equivalents ssi (F∧¬G) V (G∧¬F) insat
