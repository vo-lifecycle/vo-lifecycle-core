package org.volifecycle.lifecycle.vo;

import java.util.Calendar;

/**
 * Lifecycle change value object
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class LifeCycleChange {
	/**
	 * life cycle id
	 */
	private String lifeCycleId;

	/**
	 * transition id
	 */
	private String transitionId;

	/**
	 * state in
	 */
	private String stateIn;

	/**
	 * state out
	 */
	private String stateOut;

	/**
	 * value object id
	 */
	private String valueObjectId;

	/**
	 * value object type
	 */
	private String valueObjectType;

	/**
	 * date
	 */
	private Calendar date;

	/**
	 * @return the lifeCycleId
	 */
	public String getLifeCycleId() {
		return lifeCycleId;
	}

	/**
	 * @param lifeCycleId
	 *            the lifeCycleId to set
	 */
	public void setLifeCycleId(String lifeCycleId) {
		this.lifeCycleId = lifeCycleId;
	}

	/**
	 * @return the transitionId
	 */
	public String getTransitionId() {
		return transitionId;
	}

	/**
	 * @param transitionId
	 *            the transitionId to set
	 */
	public void setTransitionId(String transitionId) {
		this.transitionId = transitionId;
	}

	/**
	 * @return the stateIn
	 */
	public String getStateIn() {
		return stateIn;
	}

	/**
	 * @param stateIn
	 *            the stateIn to set
	 */
	public void setStateIn(String stateIn) {
		this.stateIn = stateIn;
	}

	/**
	 * @return the stateOut
	 */
	public String getStateOut() {
		return stateOut;
	}

	/**
	 * @param stateOut
	 *            the stateOut to set
	 */
	public void setStateOut(String stateOut) {
		this.stateOut = stateOut;
	}

	/**
	 * @return the valueObjectId
	 */
	public String getValueObjectId() {
		return valueObjectId;
	}

	/**
	 * @param valueObjectId
	 *            the valueObjectId to set
	 */
	public void setValueObjectId(String valueObjectId) {
		this.valueObjectId = valueObjectId;
	}

	/**
	 * @return the valueObjectType
	 */
	public String getValueObjectType() {
		return valueObjectType;
	}

	/**
	 * @param valueObjectType
	 *            the valueObjectType to set
	 */
	public void setValueObjectType(String valueObjectType) {
		this.valueObjectType = valueObjectType;
	}

	/**
	 * @return the date
	 */
	public Calendar getDate() {
		return date;
	}

	/**
	 * @param date
	 *            the date to set
	 */
	public void setDate(Calendar date) {
		this.date = date;
	}
}
