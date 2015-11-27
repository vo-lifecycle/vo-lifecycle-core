package org.volifecycle.utils;

import org.dozer.DozerBeanMapper;

/**
 * A factory for a DozerBeanMapper instance.
 * 
 * @author Idriss Neumann <idriss.neumann@capgemini.com>
 *
 */
public class DozerBeanMapperFactory {
    private static DozerBeanMapper mapper;

    /**
     * Return a single instance of DozerBeanMapper.
     * 
     * @return DozerBeanMapper
     */
    public static DozerBeanMapper getInstance() {
        if (null == mapper) {
            mapper = new DozerBeanMapper();
        }

        return mapper;
    }

    /**
     * Private constructor.
     */
    private DozerBeanMapperFactory() {

    }
}
