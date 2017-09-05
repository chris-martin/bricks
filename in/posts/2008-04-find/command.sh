find -name "*.java" \
    | xargs wc -l   \
    | sort -nr      \
    | head -n 5     \
    | sed -r -e 's/\.\/(.*\/)*//g'
