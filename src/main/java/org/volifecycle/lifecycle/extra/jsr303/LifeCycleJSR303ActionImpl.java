package org.volifecycle.lifecycle.extra.jsr303;

import static org.volifecycle.event.EventBuilder.build;

import java.util.Map;
import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.Validator;

import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.LogEventManagerImpl;
import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * JSR303 implementation of action.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class LifeCycleJSR303ActionImpl<T, A extends LifeCycleAdapter<T>> implements LifeCycleActionJSR303<T> {
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
    public String getResult(T valueObject, Map<String, Object> actionStorage) {
        return getResult(valueObject, actionStorage, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getResult(T valueObject, Map<String, Object> actionStorage, Set<ConstraintViolation<T>> constraintViolations) {
        Validator validator = CustomValidatorFactory.SINGLE_INSTANCE.getValidator();
        Set<ConstraintViolation<T>> violations = validator.validate(valueObject);

        if (null == evtManager) {
            evtManager = new LogEventManagerImpl();
        }

        if (violations.size() > 0) {
            if (null != constraintViolations) {
                constraintViolations.addAll(violations);
            }

            for (ConstraintViolation<T> violation : violations) {
                String message = violation.getRootBeanClass().getSimpleName() + "." + violation.getPropertyPath() + " " + violation.getMessage();
                Event evt = build(valueObject, adapter, LifeCycleConstants.EVENT_TYPE_JSR303_FAILURE, message, additionnalInformations, null, null);
                evtManager.logEvent(evt);
            }

            return Boolean.FALSE.toString();
        }

        return Boolean.TRUE.toString();
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
