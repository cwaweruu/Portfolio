USE spotify_music;
# Top artists by streams #
SELECT artist_name, SUM(streams) AS total_streams
FROM spotify
GROUP BY artist_name
ORDER BY total_streams DESC
LIMIT 10;

# Bottom artists by streams #
SELECT artist_name, SUM(streams) AS total_streams
FROM spotify
GROUP BY artist_name
ORDER BY total_streams ASC
LIMIT 10;

## Month with the highest released music##
SELECT released_month, COUNT(*) AS num_songs_released
FROM spotify
GROUP BY released_month
ORDER BY num_songs_released DESC;

# Dancability, energy and acousticness of top songs 
SELECT t.track_name, t.artist_name, t.`danceability_%`, t.`energy_%`, t.`acousticness_%`, t. `instrumentalness_%`, t. `liveness_%`, t.`speechiness_%`
FROM spotify t
JOIN (
    SELECT track_name
    FROM spotify
    ORDER BY streams DESC
    LIMIT 10
) top_songs
ON t.track_name = top_songs.track_name;

# Dancability, energy and acousticness of bottom songs 
SELECT t.track_name, t.artist_name, t.`danceability_%`, t.`energy_%`, t.`acousticness_%`, t. `instrumentalness_%`, t. `liveness_%`, t.`speechiness_%`
FROM spotify t
JOIN (
    SELECT track_name
    FROM spotify
    ORDER BY streams ASC
    LIMIT 10
) bottom_songs
ON t.track_name = bottom_songs.track_name;

# Popular artists
SELECT artist_name, SUM(streams) AS total_streams
FROM spotify
GROUP BY artist_name;

# Dancable songs correlation with Beat per minute
SELECT
    CASE
        WHEN bpm BETWEEN 65 AND 84 THEN '65-84 BPM'
        WHEN bpm BETWEEN 85 AND 104 THEN '85-104 BPM'
        WHEN bpm BETWEEN 105 AND 124 THEN '105-124 BPM'
        WHEN bpm BETWEEN 125 AND 144 THEN '125-144 BPM'
        WHEN bpm BETWEEN 145 AND 164 THEN '145-164 BPM'
        WHEN bpm BETWEEN 165 AND 184 THEN '165-184 BPM'
        WHEN bpm BETWEEN 185 AND 204 THEN '185-204 BPM'
        ELSE '205+ BPM'
    END AS bpm_range,
    AVG(`danceability_%`) AS avg_danceability
FROM spotify
GROUP BY bpm_range
ORDER BY avg_danceability;

# Solo artists vs Multiple Artists
SELECT
    CASE
        WHEN artist_count = 1 THEN 'Solo Artist'
        ELSE 'Multiple Artists'
    END AS artist_group,
    AVG(streams) AS avg_streams,
    AVG(in_spotify_charts) AS avg_chart_rank
FROM spotify
GROUP BY artist_group;




    

