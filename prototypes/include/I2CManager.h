#ifndef I2CMANAGER_H_
#define I2CMANAGER_H_

#include <bitset>
#include <Wire.h>

#include "Scheduler.h"

#define I2CMANAGER_DEFAULT_INTER_SCAN_PERIOD 1000
#define I2CMANAGER_DEFAULT_INTRA_SCAN_PERIOD 10

/**
 * @brief Responsible for discovering the connectivity status of I2C peripherals.
 * @verbatim
 *        @@ <- Intra Scan Period
 *           @@          &&&&&&&&&&&&&& <- Inter Scan Period
 *                                      @@
 *       |  |  | ...... | ............ |  | ....
 * Start ^  |  |        |              |  |
 *   Poll 1 ^  |        |              |  |
 *      Poll 2 ^        |              |  |
 * Finish address range ^              |  |
 *                               Start ^  |
 *                                 Poll 1 ^
 *                                          continues...
 * @endverbatim
 */
class I2CManager {
    private:
        std::bitset<128> mAddressStatus;
        Scheduler mScheduler;
        ScheduleId mInterScanScheduleId;
        ScheduleId mIntraScanScheduleId;
        uint8_t mCurPollingAddress;
        TwoWire *mWire;
        bool mDidTransmit;
        std::shared_ptr<std::function<void(uint8_t)>> mOnChangeCallback;

        void poll();

    public:
        I2CManager(
            TwoWire *wire,
            /**
             * Time betweeen the start and end of a full poll across the address
             * range.
             */
            Duration interScanPeriod = I2CMANAGER_DEFAULT_INTER_SCAN_PERIOD,
            /**
             * Time between scanning single addresses within a full poll.
             */
            Duration intraScanPeriod = I2CMANAGER_DEFAULT_INTRA_SCAN_PERIOD
        );
        void printReport(Stream *stream);
        /**
         * @brief Standard Arduino style `loop` function.
         *
         * Call this from your `loop` function. This function is non-blocking
         * (e.g., avoids the use of `delay`).
         *
         * Manages auto-discovery of I2C device connection status.
         */
        void loop();
        /**
         * @brief Set a callback for when connectivity status changes.
         *
         * The callback will receive the bus address that changed.
         */
        void setCallback(std::shared_ptr<std::function<void(uint8_t)>> f);
        /**
         * Check whether a peripheral is connected at a particular bus address.
         */
        bool isConnected(uint8_t busAddr);
};

#endif
