#https://github.com/openmaptiles/openmaptiles-tools/blob/master/bin/import-diff
docker run --rm --name  imposm3-import-update \
     -v ${PWD}/import:/import \
    -v ${PWD}/mapping:/mapping \
    -v ${PWD}/update:/update \
    -v ${PWD}/diff:/diff \
    -v ${PWD}/config:/config \
    -e PGHOST="localhost" \
    -e PGPORT=25432 \
    -e PGDATABASE="osm_db" -e PGUSER="docker" -e PGPASSWORD="docker" \
    -e IMPOSM_MAPPING_FILE=/mapping/imposm3-mapping.json \
    -e IMPOSM_CACHE_DIR=/cache \
    -e IMPOSM_DIFF_DIR=/diff \
    -e IMPOSM_CONFIG_FILE=/config/repl_config.json \
    --net=host \
    openmaptiles/openmaptiles-tools import-update
