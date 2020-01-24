mydir="$(dirname "$BASH_SOURCE")"
cd $mydir
docker build -t gregoryquick/scratch .
docker run -t -i \
 -v $mydir/data:/data \
 gregoryquick/scratch