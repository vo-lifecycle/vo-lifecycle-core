package org.volifecycle.lifecycle.extra.jsr303;

import java.util.Map;
import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.Validator;

import org.volifecycle.common.AbstractLifeCycle;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.lifecycle.LifeCycleAction;
import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * JSR303 implementation of action.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class LifeCycleJSR303ActionImpl<T, A extends LifeCycleAdapter<T>> extends AbstractLifeCycle<T> implements LifeCycleAction<T> {
	/**
	 * The adapter
	 */
	protected A adapter;

	/**
	 * The event manager
	 */
	protected EventManager evtManager;

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
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, String> getAdditionnalInformations() {
		return additionnalInformations;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getResult(T valueObject) {
		return getResult(valueObject, null);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getResult(T valueObject, Map<String, Object> actionStorage) {
		Validator validator = CustomValidatorFactory.SINGLE_INSTANCE.getValidator();
		Set<ConstraintViolation<T>> constraintViolations = validator.validate(valueObject);

		if (constraintViolations.size() > 0) {
			for (ConstraintViolation<T> violation : constraintViolations) {
				String message = violation.getRootBeanClass().getSimpleName() + "." + violation.getPropertyPath() + " " + violation.getMessage();
				logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_JSR303_FAILURE, message, additionnalInformations);
			}
			return LifeCycleConstants.FALSE;
		}

		return LifeCycleConstants.TRUE;
	}

	/**
	 * @return the adapter
	 */
	public A getAdapter() {
		return adapter;
	}

	/**
	 * @param adapter
	 *            the adapter to set
	 */
	public void setAdapter(A adapter) {
		this.adapter = adapter;
	}

	/**
	 * @return the evtManager
	 */
	public EventManager getEvtManager() {
		return evtManager;
	}

	/**
	 * @param evtManager
	 *            the evtManager to set
	 */
	public void setEvtManager(EventManager evtManager) {
		this.evtManager = evtManager;
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
}
