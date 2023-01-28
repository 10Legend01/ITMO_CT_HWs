package com.legend.android_9dz

import android.content.ClipboardManager
import android.content.Context
import android.content.Context.CLIPBOARD_SERVICE
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Looper
import androidx.test.core.app.ApplicationProvider.getApplicationContext
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry.getInstrumentation
import androidx.test.uiautomator.By
import androidx.test.uiautomator.UiDevice
import androidx.test.uiautomator.Until
import org.junit.Assert.*
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import androidx.test.uiautomator.UiObject2
import java.lang.Thread.sleep

class AppButtons(mDevice: UiDevice, pack: String = "com.legend.android_9dz") {
    val textTable: UiObject2 = mDevice.findObject(By.res(pack, "table_text"))
    val butCopy: UiObject2 = mDevice.findObject(By.res(pack, "but_copy"))
    val butMultiply: UiObject2 = mDevice.findObject(By.res(pack, "but_multiply"))
    val butDivide: UiObject2 = mDevice.findObject(By.res(pack, "but_divide"))
    val butPlus: UiObject2 = mDevice.findObject(By.res(pack, "but_plus"))
    val butSubtract: UiObject2 = mDevice.findObject(By.res(pack, "but_subtract"))
    val butEquals: UiObject2 = mDevice.findObject(By.res(pack, "but_equals"))
    val butDelete: UiObject2 = mDevice.findObject(By.res(pack, "but_delete"))
    val butReset: UiObject2 = mDevice.findObject(By.res(pack, "but_reset"))
    val butComma: UiObject2 = mDevice.findObject(By.res(pack, "but_comma"))
    val but0: UiObject2 = mDevice.findObject(By.res(pack, "but_0"))
    val but1: UiObject2 = mDevice.findObject(By.res(pack, "but_1"))
    val but2: UiObject2 = mDevice.findObject(By.res(pack, "but_2"))
    val but3: UiObject2 = mDevice.findObject(By.res(pack, "but_3"))
    val but4: UiObject2 = mDevice.findObject(By.res(pack, "but_4"))
    val but5: UiObject2 = mDevice.findObject(By.res(pack, "but_5"))
    val but6: UiObject2 = mDevice.findObject(By.res(pack, "but_6"))
    val but7: UiObject2 = mDevice.findObject(By.res(pack, "but_7"))
    val but8: UiObject2 = mDevice.findObject(By.res(pack, "but_8"))
    val but9: UiObject2 = mDevice.findObject(By.res(pack, "but_9"))
}

@RunWith(AndroidJUnit4::class)
class AndroidTest {

    private fun getLauncherPackageName(): String? {
        // Create launcher Intent
        val intent = Intent(Intent.ACTION_MAIN)
        intent.addCategory(Intent.CATEGORY_HOME)

        // Use PackageManager to get the launcher package name
        val pm = getApplicationContext<Context>().packageManager
        val resolveInfo = pm.resolveActivity(intent, PackageManager.MATCH_DEFAULT_ONLY)
        return resolveInfo!!.activityInfo.packageName
    }

    private val BASIC_SAMPLE_PACKAGE = "com.legend.android_9dz"
    private val LAUNCH_TIMEOUT: Long = 8000

    private lateinit var mDevice: UiDevice

    private lateinit var buttons: AppButtons

    private fun buttonInit() {
        buttons = AppButtons(mDevice)
    }

    private fun setHorizontalOrientation() {
        mDevice.setOrientationLeft()
        sleep(250)
        buttonInit()
    }

    private fun setVerticalOrientation() {
        mDevice.setOrientationNatural()
        sleep(250)
        buttonInit()
    }

    private fun getClipboard(): String {
        return try {
            (getApplicationContext<Context>().getSystemService(CLIPBOARD_SERVICE) as ClipboardManager)
                .primaryClip?.getItemAt(0)?.text.toString()
        } catch (e: Exception) {
            "NaN"
        }
    }

    @Before
    fun setUp() {
        // Initialize UiDevice instance
        mDevice = UiDevice.getInstance(getInstrumentation())

        // Start from the home screen
        mDevice.pressHome()

        // Wait for launcher
        val launcherPackage = getLauncherPackageName()
        assertNotNull(launcherPackage)
        mDevice.wait(Until.hasObject(By.pkg(launcherPackage).depth(0)), LAUNCH_TIMEOUT)

        // Launch the blueprint app
        val context = getApplicationContext<Context>()
        val intent = context.packageManager.getLaunchIntentForPackage(BASIC_SAMPLE_PACKAGE)
        intent!!.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK) // Clear out any previous instances
        context.startActivity(intent)

        // Wait for the app to appear
        mDevice.wait(Until.hasObject(By.pkg(BASIC_SAMPLE_PACKAGE).depth(0)), LAUNCH_TIMEOUT)

        setVerticalOrientation()
    }

    @Test
    fun screenRotation() {
        buttons.but2.click()
        buttons.but3.click()

        assertEquals(23.0, buttons.textTable.text.toDouble(), 0.0)

        setHorizontalOrientation()

        assertEquals(23.0, buttons.textTable.text.toDouble(), 0.0)
    }

    @Test
    fun clipboardAccess() {
        Looper.prepare()
        buttons.but2.click()
        buttons.butPlus.click()
        buttons.but2.click()
        buttons.butEquals.click()

        assertEquals(4.0, buttons.textTable.text.toDouble(), 0.0)

        buttons.butCopy.click()
        sleep(50)

        assertEquals(4.0, getClipboard().toDouble(), 0.0)

        buttons.butPlus.click()
        buttons.but2.click()
        buttons.butEquals.click()
        sleep(50)

        assertEquals(6.0, buttons.textTable.text.toDouble(), 0.0)
        assertEquals(4.0, getClipboard().toDouble(), 0.0)

        buttons.butCopy.click()
        sleep(50)

        assertEquals(6.0, getClipboard().toDouble(), 0.0)
    }

    @Test
    fun scenario() {
        buttons.but1.click()
        buttons.but3.click()
        buttons.but5.click()
        buttons.but7.click()
        buttons.but9.click()

        assertEquals(13579.0, buttons.textTable.text.toDouble(), 0.0)

        buttons.butMultiply.click()
        buttons.but2.click()
        buttons.but4.click()
        buttons.but6.click()
        buttons.but8.click()
        buttons.but0.click()
        buttons.butEquals.click()

        assertEquals(335129720.0, buttons.textTable.text.toDouble(), 0.0)
        setHorizontalOrientation()
        assertEquals(335129720.0, buttons.textTable.text.toDouble(), 0.0)

        buttons.butDivide.click()
        buttons.but9.click()
        buttons.but7.click()
        buttons.but5.click()
        buttons.but3.click()
        buttons.but1.click()
        buttons.butEquals.click()

        assertEquals(3436.13538259, buttons.textTable.text.toDouble(), 0.00001)
        setVerticalOrientation()
        assertEquals(3436.13538259, buttons.textTable.text.toDouble(), 0.00001)

        buttons.butReset.click()
        buttons.but1.click()
        buttons.butComma.click()
        buttons.but1.click()
        buttons.but1.click()

        assertEquals(1.11, buttons.textTable.text.toDouble(), 0.0)

        buttons.butDelete.click()

        assertEquals(1.1, buttons.textTable.text.toDouble(), 0.0)

        buttons.butPlus.click()
        buttons.but2.click()
        buttons.butComma.click()
        buttons.but2.click()
        buttons.butEquals.click()

        assertFalse(buttons.textTable.text.toDouble() == 3.3)
        assertEquals(3.3, buttons.textTable.text.toDouble(), 0.00001)
        setHorizontalOrientation()
        assertFalse(buttons.textTable.text.toDouble() == 3.3)
        assertEquals(3.3, buttons.textTable.text.toDouble(), 0.00001)

        buttons.butPlus.click()
        buttons.butSubtract.click()
        buttons.but1.click()
        buttons.butEquals.click()

        assertFalse(buttons.textTable.text.toDouble() == 2.3)
        assertEquals(2.3, buttons.textTable.text.toDouble(), 0.00001)
    }

}
