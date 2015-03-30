package org.volifecycle.event.vo;

import java.util.List;

/**
 * Diff event class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class DiffEvent extends Event {
	private static final long serialVersionUID = 1L;

	/**
	 * List of diff properties
	 */
	List<DiffProperty> diffProperties;

	/**
	 * @return the diffProperties
	 */
	public List<DiffProperty> getDiffProperties() {
		return diffProperties;
	}

	/**
	 * @param diffProperties
	 *            the diffProperties to set
	 */
	public void setDiffProperties(List<DiffProperty> diffProperties) {
		this.diffProperties = diffProperties;
	}
}
