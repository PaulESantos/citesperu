# Preparación de datos (pendiente)

Las fuentes candidatas están descritas en [FUENTES.md](../docs/FUENTES.md)
y el contrato propuesto en [PLAN.md](../docs/PLAN.md).

El flujo será: descargar originales y registrar su checksum; importar fauna
desde XLS y extraer flora desde PDF; normalizar conservando texto y referencias
de origen; validar contra las fuentes; generar los datasets `.rda`.

Los scripts previstos son `import_fauna.R`, `parse_flora.R` y `DATASET.R`.
Aún no se han implementado ni se han incorporado datos. Las actualizaciones
empaquetadas se producirán aquí y se distribuirán como nuevas versiones;
la futura función de usuario `update_cites_pe()` trabajará con una caché.
