package org.volifecycle.lifecycle.impl;

import org.volifecycle.lifecycle.LifeCycleChecker;

/**
 * Implementation of checker
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject
 */
public abstract class LifeCycleCheckerImpl<T> implements LifeCycleChecker<T> {
	/**
	 * Id which is used for forced the result of this checker
	 */
	protected String id;

	/**
	 * Description
	 */
	protected String description;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getId() {
		return id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getDescription() {
		return description;
	}

	/**
	 * @param description
	 *            the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}
}
