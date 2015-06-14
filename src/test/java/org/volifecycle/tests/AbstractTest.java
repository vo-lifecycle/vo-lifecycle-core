package org.volifecycle.tests;

import static org.junit.Assert.fail;

import java.io.File;

/**
 * Abstract test class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public abstract class AbstractTest {
	/**
	 * Fail with exception
	 */
	public void failWithException(Exception e) {
		e.printStackTrace();
		fail("Unexpected exception : " + e.getMessage());
	}

	/**
	 * Get test data dir (with / at the end)
	 * 
	 * @return String
	 */
	public String getTestDataDir() {
		return "src" //
				+ File.separator //
				+ "test" //
				+ File.separator //
				+ "resources" //
				+ File.separator //
				+ "data" //
				+ File.separator //
				+ this.getClass().getSimpleName() //
				+ File.separator;
	}

	/**
	 * Get test common data dir (with / at the end)
	 * 
	 * @return String
	 */
	public String getTestCommonDataDir() {
		return "src" //
				+ File.separator //
				+ "test" //
				+ File.separator //
				+ "resources" //
				+ File.separator //
				+ "data" //
				+ File.separator //
				+ "Common" //
				+ File.separator;
	}

	/**
	 * Get test resources dir (with / at the end)
	 * 
	 * @return String
	 */
	public String getTestResourcesDir() {
		return "src" //
				+ File.separator //
				+ "test" //
				+ File.separator //
				+ "resources" //
				+ File.separator;
	}

	/**
	 * Get main resources dir (with / at the end)
	 * 
	 * @return String
	 */
	public String getDataDir() {
		return "src" //
				+ File.separator //
				+ "main" //
				+ File.separator //
				+ "resources" //
				+ File.separator;
	}
}
