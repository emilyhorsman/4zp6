#include <bitset>
#include "Wire.h"

#define I2CMANAGER_DEFAULT_POLL_INTERVAL_MILLI 1000
#define I2CMANAGER_DEFAULT_SCAN_INTERVAL_MILLI 5

class I2CManager {
    private:
        std::bitset<128> mAddressStatus;
        uint32_t mLastPollTimeMilli;
        uint32_t mPollingIntervalMilli;
        uint32_t mScanIntervalMilli;
        uint8_t mCurPollingAddress;
        TwoWire *mWire;

        bool shouldPoll();
        void poll();

    public:
        I2CManager(
            TwoWire *wire,
            /**
             * Time betweeen the start and end of a full poll across the address
             * range.
             */
            uint32_t pollingInterval = I2CMANAGER_DEFAULT_POLL_INTERVAL_MILLI,
            /**
             * Time between scanning single addresses within a full poll.
             */
            uint32_t scanInterval = I2CMANAGER_DEFAULT_SCAN_INTERVAL_MILLI
        );
        void printReport(Stream *stream);
        /**
         * Call this from your `loop` function. This function is non-blocking
         * (e.g., avoids the use of `delay`).
         * 
         * Manages auto-discovery of I2C device connection status.
         */
        void loop();
};