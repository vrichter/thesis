#!/home/vrichter/bin/fish
while true;
    inotifywait -e modify *tex
    curl -k  'https://bpc:8082/view/Diss/job/Diss%20Live/build?token=livediss'
    sleep 1
end

