from pathlib import Path
import sys,pandas as pd,numpy as np
root=Path(sys.argv[1]);out=root/'output/part3'
transport=pd.read_csv(out/'wasserstein_harmonised.csv')
primary=transport[transport.support_policy=='primary_five_common_complete'].copy()
primary['descriptive_distance_rank']=primary.groupby(['reference_definition','duration_variant','event','representation']).distance_multi.rank(method='min')
primary.to_csv(out/'primary_transport_with_sensitivity_endpoints.csv',index=False)
native=primary[primary.duration_variant=='native'];span=primary[primary.duration_variant=='span']
comparison=native.merge(span,on=['model','representation','event','reference_definition'],suffixes=('_native','_span'))
comparison['distance_difference_span_minus_native']=comparison.distance_multi_span-comparison.distance_multi_native
comparison['descriptive_rank_changed']=comparison.descriptive_distance_rank_native!=comparison.descriptive_distance_rank_span
comparison.to_csv(out/'duration_convention_sensitivity.csv',index=False)
direction=[]
for (model,event,ref,variant),g in primary.groupby(['model','event','reference_definition','duration_variant']):
 assert len(g)==2 and g.support_sha256.nunique()==1
 o=g[g.representation=='original'].iloc[0];r=g[g.representation=='rate30'].iloc[0]
 direction.append(dict(model=model,event=event,reference_definition=ref,duration_variant=variant,n_trials=o.n_trials,
  support_sha256=o.support_sha256,distance_original=o.distance_multi,distance_rate30=r.distance_multi,
  distance_difference_rate30_minus_original=r.distance_multi-o.distance_multi,
  descriptive_direction='30_Hz_distribution_closer' if r.distance_multi<o.distance_multi else 'Original_distribution_closer' if o.distance_multi<r.distance_multi else 'equal'))
direction=pd.DataFrame(direction);direction.to_csv(out/'representation_distribution_contrasts.csv',index=False)
dn=direction[direction.duration_variant=='native'];ds=direction[direction.duration_variant=='span']
dc=dn.merge(ds,on=['model','event','reference_definition'],suffixes=('_native','_span'))
flips=int((dc.descriptive_direction_native!=dc.descriptive_direction_span).sum())
reference=pd.read_csv(out/'reference_event_validation.csv')
ref_unique=reference.drop_duplicates(['sub','Trial_Id'])
affected=ref_unique[ref_unique.missing_reference_labels>0]
count_change=reference[reference.N_historical!=reference.N_gap_aware]
velocity=pd.read_csv(out/'saccade_velocity_summary.csv')
reasons=pd.read_csv(out/'saccade_velocity_missingness.csv')
periods=pd.read_csv(out/'derived/trial_recording_periods.csv')
exposure=pd.DataFrame([
 dict(representation='Original',role='classical_detector_and_sampled_reference_input',source_samples=int(periods.original_n.sum()),policy='conf>0 and el_pupil>0'),
 dict(representation='Original',role='CNN_full_native_sequence',source_samples=int(periods.cnn_original_n.sum()),policy='frozen C3-B full sequence; native events retained'),
 dict(representation='30 Hz',role='classical_and_CNN_observed_frame_input',source_samples=int(periods.rate30_n.sum()),policy='archived observed-frame representation'),
 dict(representation='30 Hz',role='reference_event_construction_source',source_samples=int(periods.original_n.sum()),policy='Original sampled reference boundaries; velocity measured on 30-Hz frames')])
exposure.to_csv(out/'native_input_exposure_summary.csv',index=False)
