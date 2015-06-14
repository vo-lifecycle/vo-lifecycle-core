package org.volifecycle.lifecycle.extra.jsr303;

import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;

import org.apache.bval.jsr303.ApacheValidationProvider;

/**
 * Validation factory with apache bval.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public enum CustomValidatorFactory {
	SINGLE_INSTANCE {
		ValidatorFactory avf = Validation.byProvider(ApacheValidationProvider.class).configure().buildValidatorFactory();

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Validator getValidator() {
			return avf.getValidator();
		}
	};

	public abstract Validator getValidator();
}