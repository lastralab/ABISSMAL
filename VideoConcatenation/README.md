# Concatenating mp4 videos using ffmpeg

Extracted from: https://ottverse.com/3-easy-ways-to-concatenate-mp4-files-using-ffmpeg/ 

1. Download & Install ffmpeg from https://ffmpeg.org/download.html
2. Go to videos directory `cd <path>`
3. Add file names with full paths to a txt file: `

   ```
   $ vim fileList.txt
   file '/home/pre.mp4'
   file '/home/post.mp4'

   ````
4. Run concat command ``ffmpeg -f concat -safe 0 -i fileList.txt -c copy merged.mp4``
5. Result: Both videos merged.

Note: ffmpeg didn't work as expected in raspbian. We recommend to run it from another OS