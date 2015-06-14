package org.volifecycle.lifecycle.impl;

import java.util.Map;

import org.volifecycle.lifecycle.LifeCycleAction;

/**
 * Simple abstract action implementation
 * 
 * @author Idriss Neumann <idriss.neumann@capgemini.com>
 *
 * @param <T>
 *            the value object type
 */
public abstract class LifeCycleSimpleActionImpl<T> implements LifeCycleAction<T> {
	/**
	 * Id which is used for forced the result of this action
	 */
	protected String id;

	/**
	 * Description
	 */
	protected String description;

	/**
	 * Additionnal informations to save.
	 */
	protected Map<String, String> additionnalInformations;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getId() {
		return id;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getDescription() {
		return description;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @param description
	 *            the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * @param additionnalInformations
	 *            the additionnalInformations to set
	 */
	public void setAdditionnalInformations(Map<String, String> additionnalInformations) {
		this.additionnalInformations = additionnalInformations;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, String> getAdditionnalInformations() {
		return additionnalInformations;
	}
}
