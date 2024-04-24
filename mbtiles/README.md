
# ptncafe.maptiles.mbtiles

Demo project

## Installation
### Install tilemaker
```
git clone https://github.com/systemed/tilemaker
```
```bash
mkdir build
cd build
cmake ..
make
sudo make install
```
### install mbtileserver
```
go install github.com/consbio/mbtileserver@latest
```


## Usage
### Genarate mbtile file
pbf_to_mbtile.sh
```bash
tilemaker --input file/vietnam-latest.osm.pbf --output=file/vietnam.mbtiles --process resources/process-openmaptiles.lua
```


### Run mbtileserver
run_mbtileserver.sh
```bash
PORT=9999 TILE_DIR=./file VERBOSE=true mbtileserver --basemap-style-url styles/map_navigation_styles.json --basemap-tiles-url  https://tile.openstreetmap.org/{z}/{x}/{y}.png
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)