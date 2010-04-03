package com.loadtrend.lib.sound;

// TODO 1: Add audio service notification(master/channel volume changed).
//       See more details on MSSDK/Sample/MultiMedia/Audio/WinAudio/player.cpp
// TODO 2: Get/Set audio device  channel volume. This "todo 2" should be done after todo 1.
//       Because author should know how to notify master volume when channel volume changes.
//       Master volume and Channel volume will affect each other when either changes.

public class VistaAudioDevice {

	// device name type
	private static final int DEVICE_MAIN_NAME = 0;

	// device name type
	private static final int DEVICE_INTERFACE_NAME = 1;

	// device name type
	private static final int DEVICE_MAIN_INTERFACE_NAME = 2;

    private static final native boolean initializeAudioDevice();

    private static final native void destroyAudioDevice();

    private static final native int getCaptureDeviceListCount();

    private static final native String getCaptureListDeviceName(int index, int deviceNameType);

    private static final native String getCaptureListDeviceId(int index);

    private static final native String getDefaultCaptureDeviceId();

    private static final native boolean getCaptureListDeviceState(int index);

	// float fVolume: the range of the value from 0.0 to 1.0
	private static final native boolean setCaptureListDeviceVolume(int index, float fVolume);

	// float fVolume: the range of the value from 0.0 to 1.0. -1 means fail to get.
	private static final native float getCaptureListDeviceVolume(int index);

	// private static final native boolean GetCaptureListDeviceChannelCount(DeviceInfo *pDeviceInfo, int index, UINT *iCount);

	// float fVolume: the range of the value from 0.0 to 1.0
	// private static final native boolean GetCaptureListDeviceChannelVolume(DeviceInfo *pDeviceInfo, int index, float *pfVolume, UINT iChannel);

	// float fVolume: the range of the value from 0.0 to 1.0
	// private static final native boolean SetCaptureListDeviceChannelVolume(DeviceInfo *pDeviceInfo, int index, float fVolume, UINT iChannel);

	// equals to set device state.
    private static final native void setEndpointVisibility(String deviceId, boolean bVisibility);

    private static final native void setDefaultEndpoint(String deviceId);

	// TODO: Finish the struct-class mapping when I have time.
    // private static final native void getMixFormat(String deviceId, WAVEFORMATEX **ppWaveFormatEx);

	private static final native String testString(String string);

	static {
		System.loadLibrary("audio-device-java");
	}

	public static void main(String[] args) {
		String passedOutValue = VistaAudioDevice.testString("Passed In Value ÷–Œƒ≤‚ ‘.");
		System.out.println("Passed Out Value from C: " + passedOutValue);
		
		if (VistaAudioDevice.initialize()) {
			AudioDevice[] audioDevices = VistaAudioDevice.getCaptureDevices();
			for (int i = 0; i < audioDevices.length; i++) {
				System.out.println("Index: " + audioDevices[i].deviceIndex);
				System.out.println("Id: " + audioDevices[i].deviceId);
				System.out.println("Main Name: " + audioDevices[i].deviceMainName);
				System.out.println("Interface Name: " + audioDevices[i].deviceInterfaceName);
				System.out.println("Main Interface Name: " + audioDevices[i].deviceMainInterfaceName);
				System.out.println("Master Volume: " + audioDevices[i].deviceVolume);
				System.out.println("Default: " + audioDevices[i].deviceDefault);
				System.out.println("State: " + audioDevices[i].deviceState);
			}
			System.out.println("Set 0 Device Volume to 0.1f.");
			VistaAudioDevice.setVolume(audioDevices[0], 0.1f);
			System.out.println("Set 0 Device State to reverse.");
			VistaAudioDevice.setState(audioDevices[0], !audioDevices[0].deviceState);
			System.out.println("Set 0 Device as default device.");
			VistaAudioDevice.setDefault(audioDevices[0], audioDevices);
		}
		VistaAudioDevice.destory();
	}

	/**
	 * The first method should be invoked on VistaAudioDevice before inoking other methods.
	 */
	public static final boolean initialize() {
		return VistaAudioDevice.initializeAudioDevice();
	}

	/**
	 * The last method should be invoked on VistaAudioDevice after inoking other methods.
	 */
	public static final void destory() {
		VistaAudioDevice.destroyAudioDevice();
	}

	/**
	 * Get all the capture audio devices.
	 * device volume will return a float which is equal to [0.0f, 1.0f]
	 * If fail to get the volume, it return -1.0f
	 */
	public static final AudioDevice[] getCaptureDevices() {
		int iCount = VistaAudioDevice.getCaptureDeviceListCount();
		String defaultDeviceId = VistaAudioDevice.getDefaultCaptureDeviceId();
		AudioDevice[] audioDevices = new AudioDevice[iCount];
		for (int i = 0; i < iCount; i++) {
			audioDevices[i] = new VistaAudioDevice().new AudioDevice();
			audioDevices[i].deviceIndex = i;
			audioDevices[i].deviceId = VistaAudioDevice.getCaptureListDeviceId(i);
			audioDevices[i].deviceMainName = VistaAudioDevice.getCaptureListDeviceName(i, VistaAudioDevice.DEVICE_MAIN_NAME);
			audioDevices[i].deviceInterfaceName = VistaAudioDevice.getCaptureListDeviceName(i, VistaAudioDevice.DEVICE_INTERFACE_NAME);
			audioDevices[i].deviceMainInterfaceName = VistaAudioDevice.getCaptureListDeviceName(i, VistaAudioDevice.DEVICE_MAIN_INTERFACE_NAME);
			audioDevices[i].deviceVolume = VistaAudioDevice.getCaptureListDeviceVolume(i); // return -1.0f means failed.
			audioDevices[i].deviceState = VistaAudioDevice.getCaptureListDeviceState(i);
			if (audioDevices[i].deviceId != null && audioDevices[i].deviceId.equals(defaultDeviceId)) {
				audioDevices[i].deviceDefault = true;
			}
		}
		return audioDevices;
	}
	
	/**
	 * Set audio device volume.
	 * @param device The specified audio device of which volume should be changed.
	 * @param fVolume should be equal to [0.0f, 1.0f]
	 * @return true/false to indicate: success/fail to set volume.
	 */
	public static final boolean setVolume(AudioDevice device, float fVolume) {
		return VistaAudioDevice.setCaptureListDeviceVolume(device.getDeviceIndex(), fVolume);
	}
	
	/**
	 * Set audio device state.
	 * @param device The specified audio device of which state should be changed.
	 * @param bVisibility true/false to indicate: enable/disable the device.
	 * @return void.
	 */
	public static final void setState(AudioDevice device, boolean bVisibility) {
		VistaAudioDevice.setEndpointVisibility(device.getDeviceId(), bVisibility);
	}
	
	/**
	 * Set defaul audio device.
	 * @param device The specified audio device should be set to default.
	 * @param audioDevices This is an array of AudioDevice which exactly comes from VistaAudioDevice.getCaptureDevices() method.
	 * @return void.
	 */
	public static final void setDefault(AudioDevice device, AudioDevice[] audioDevices) {
		for (int i = 0; i < audioDevices.length; i++) {
			if (audioDevices[i].deviceDefault) {
				if (audioDevices[i] == device) return;
		        VistaAudioDevice.setDefaultEndpoint(device.getDeviceId());
				audioDevices[i].deviceDefault = false;
				device.deviceDefault = true;
			}
		}
	}

	public class AudioDevice {
		private int deviceIndex = 0;
		private String deviceId = null;
		private String deviceMainName = null;
		private String deviceInterfaceName = null;
		private String deviceMainInterfaceName = null;
		private float deviceVolume = 0.0f; // return -1.0f means failed.
		private boolean deviceState = false;
		private boolean deviceDefault = false;
		public int getDeviceIndex() {
			return deviceIndex;
		}
		public String getDeviceId() {
			return deviceId;
		}
		public String getDeviceMainName() {
			return deviceMainName;
		}
		public String getDeviceInterfaceName() {
			return deviceInterfaceName;
		}
		public String getDeviceMainInterfaceName() {
			return deviceMainInterfaceName;
		}
		public float getDeviceVolume() {
			return deviceVolume;
		}
		public boolean getDeviceState() {
			return deviceState;
		}
		public boolean getDeviceDefault() {
			return deviceDefault;
		}
	}
}

