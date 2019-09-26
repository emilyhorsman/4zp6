# LIS3DH

- `STATUS_REG` before checking data (proven unnecessary in the end)
- Setting accelerometer range (https://github.com/adafruit/Adafruit_LIS3DH/blob/master/examples/acceldemo/acceldemo.ino#L36)
- Autoincrement support but as far as processor is concerned this is simply a different register/num bytes
- Number of things to write on setup
- Do we need to set a current config or have IDs for configs that can then be updated/removed/pushed?